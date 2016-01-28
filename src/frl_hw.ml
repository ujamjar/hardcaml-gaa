module Types = struct

  type module_defn = 
    {
      mdefn_name : string;
      mdefn_prims : primitive list;
      mdefn_rules : rule list;
    }

  and primitive = 
    | Reg of string
    (*| Memory of string * int*)
    (*| Fifo ... *)

  and exp = HardCaml.Signal.Comb.t
    (*| Emethod_call of method_call (* read method *) 
        need to insert method calls which return a wire somehow *)

  and rule = 
    {
      rule_name : string;
      rule_guard : exp;
      rule_action : action list;
    }

  and action = 
    | Reg_set of exp
    | Mem_set of exp * exp

end

module Dsl = struct
  
  open Types

  open HardCaml.Signal.Comb
  open HardCaml.Signal.Seq

  let rule rule_name ~guard rule_action = 
    { rule_name; rule_guard=guard; rule_action }

  let _module ~prims ~rules name = 
    {
      mdefn_name = name;
      mdefn_prims = prims;
      mdefn_rules = rules;
    }

  let reg name width = 
    let d = wire width in
    let en = wire 1 in
    let q = reg r_sync en d in
    object
      method d = d
      method en = en
      method q = q
      method name = name
    end

end

(* A simplified GAA model
 * ======================
 *
 * Only supports registers but is a lot simpler,
 * integrates easily with hardcaml and should allow
 * use to explore a basic scheduler.
 *
 * We could probably extend this to include FIFOs and 
 * arrays, however, probably the right thing to do is
 * to extend it with methods which are the superset
 * of this functionality we really want.
 *
 * Rules and State
 * ===============
 *
 * We define an interface which includes all the
 * register state.  Rules are defined a function
 * which takes a (readable) record of the state,
 * and returns record with the guard and action.
 *
 * The action is specified as a state record where
 * an empty element means no state update.
 *
 * Inputs/Outputs
 * ==============
 *
 * In the GAA model inputs are registers with
 * only a get method, and outputs are just like
 * registers.  We'll simplify this a little
 * and not handle inputs specifically - they can
 * just be hardcaml inputs.  Outputs we
 * will treat as some hardcaml function of the
 * state.
 *
 * Lets
 * ====
 * 
 * I am not totally sure of the details of lets
 * in the GAA model, but they could be conceptually
 * simple - just global bindings built from the state
 * (and maybe inputs).  
 *
 * Adding global lets would be easy enough (as a function
 * of the state) unless we look into combining rules 
 * with something like a EHR, where the global lets would
 * need to be calculated on a per rule basis depending on
 * how they are combined.
 *
 * Rule parallelism
 * ================
 *
 * Rules may be fired in parallel according to some criteria
 * on the state they read/write and guards.
 *
 * For each rule (action and guard) we define two functions
 *
 * - D() - 'domain' the state elements that are read
 * - R() - 'range' the state elements are are written
 *
 * We also define
 *
 * - G() - guard
 * - A() - action
 *
 * There are 3 types of rules to consider 
 *
 * - CF - conflict free
 * - ME - mutually exclusive
 * - SC - sequentially composable
 *
 * CF
 * ==
 *
 * 2 rules ('a' and 'b') are CF if 
 * 
 * - D(a) n R(b) = {} rule a reads nothing that rule b writes
 * - D(b) n R(a) = {} rule b reads nothing that rule a writes
 * - R(a) n R(b) = {} rule a writes nothing that rule b writes
 *
 * ME
 * ==
 *
 * ME means 2 rules will never both fire at the same time ie
 *
 * - !( G(a) & G(b) )
 *
 * ME is trivially conflict free (since they can never both
 * be true in the same cycle, even if they are scheduled
 * together).  Plan is to eventually use a sat solver to
 * determine ME (though it could be slow..!).
 *
 * SC
 * ==
 *
 * SC says, roughly, you can execute rule a then rule b,
 * where rule b overwrites any conflicting updates from 
 * rule a.  This is true if the state rule b reads is 
 * not updated by rule a.
 *
 * - D(b) ~u R(a)
 *
 * Note; with FIFOs and arrays there are undefined cases
 * which exclude SC.
 *
 * CF scheduler
 * ============
 *
 * 
 *
 *)

open HardCaml.Signal.Comb

(* ocamlgraph modules *)
module G = Graph.Persistent.Graph.Concrete(struct 
  type t = int 
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end)
module D = Graph.Persistent.Digraph.Concrete(struct 
  type t = int 
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end)
module Connected = Graph_ex.Components.Undirected(G)
module Clique = Graph.Clique.Bron_Kerbosch(G)
module Gviz = Graph.Graphviz.Dot(struct
  include G
  let vertex_name v = "\"" ^ string_of_int v ^ "\""
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = [ ]
  let get_subgraph _ = None
end)

let gviz fname g = 
  let f = open_out fname in
  Gviz.output_graph f g;
  close_out f

type 'a rule = 
  {
    guard : t;
    action : 'a;
  }

module Schedule(S : HardCaml.Interface.S) = struct

  open HardCaml.Signal.Types

  type rules = (string * (t S.t -> t S.t rule)) list

  module StrSet = Set.Make(String)
  module StrMap = Map.Make(String)

  let state () = 
    let s = S.map (fun (_,b) -> wire b) S.t in
    let map = List.fold_left (fun m w -> UidMap.add (uid w) w m) UidMap.empty S.(to_list s) in
    s, map

  (* computation of domain and ranges.
   * XXX we recompute various domains for different functions.
   *     should really store them in a (lazy?) table *)

  let rec domain_of_expr state set expr = 
    match UidMap.find (uid expr) state with
    | exception _ -> 
      List.fold_left (domain_of_expr state) set (deps expr)
    | _ -> 
      UidSet.add (uid expr) set

  let domain_of_guard state rule = domain_of_expr state UidSet.empty rule.guard

  let domain_of_action state rule = 
    List.fold_left (domain_of_expr state) UidSet.empty S.(to_list rule.action)

  let domain state rule = 
    UidSet.union
      (domain_of_guard state rule)
      (domain_of_action state rule)

  let range state rule = 
    let add_to_range set (w,a) =
      if a = Signal_empty then set
      else UidSet.add (uid w) set
    in
    List.fold_left add_to_range UidSet.empty 
      S.(to_list @@ map2 (fun a b -> a,b) state rule.action)

  let empty_inter s0 s1 = UidSet.(is_empty @@ inter s0 s1) 

  let conflict_free (state,state_map) r1 r2 = 
    let domain_r1, domain_r2 = domain state_map r1, domain state_map r2 in
    let range_r1, range_r2 = range state r1, range state r2 in
    empty_inter domain_r1 range_r2 &&
    empty_inter domain_r2 range_r1 &&
    empty_inter range_r1 range_r2 

  let mutually_exclusive (state,state_map) r1 r2 = 
    (* XXX options
      * - use a sat solver!
      * - simple analysis based on conjunction of top level literals 
      * - user guided annotations *)
    false

  let sequentially_composable (state,state_map) r1 r2 = 
    empty_inter (domain state_map r2) (range state r1)

  type constraints = 
    {
      (* edge *)
      i1 : int;
      i2 : int;
      (* constraints *)
      cf : bool;
      me : bool;
      sc12 : bool;
      sc21 : bool;
    }

  (* XXX could mangle the names, but we'll address this properly when we 
   * consider module hierarchies *)
  let check_rule_names_unique rules = 
    if List.length rules <> 
      (StrSet.cardinal (StrSet.of_list (List.map fst rules))) then begin
      failwith "rule names must be unique"
    end

  (* switch these hacked up graph algorithms over to ocamlgraph.  Apart from
   * generating the feeback arc set (see Eades algorithm) it should cover all
   * we need *)

  let instantiate_rules rules = 
    check_rule_names_unique rules;
    let state = state () in
    let rules = List.mapi (fun i (n,r) -> n, i, r (fst state)) rules in
    state, rules

  let print_uid_set s = 
    List.iter (Printf.printf "%Li ") (UidSet.elements s)

  let show_rule_dr (st,stm) (n,i,r) = 
    let open Printf in
    printf "%s[%i] d={ " n i;
    print_uid_set (domain stm r);
    printf "} r={ ";
    print_uid_set (range st r);
    printf "}\n"

  let show_rule_constraints (n1,i1,r1) (n2,i2,r2) cf me sc12 sc21 = 
    let open Printf in
    let b = function true -> "\xe2\x9c\x93" | false -> "\xe2\x9c\x97" in
    printf "%s[%i] <-> %s[%i] " n1 i1 n2 i2;
    printf "cf=%s me=%s sc12=%s sc21=%s\n" 
      (b cf) (b me) (b sc12) (b sc21)

  let pairs l = 
    let rec pairs = function [] -> [] | h::t -> (List.map (fun t -> h,t) t) :: pairs t in
    List.concat (pairs l) 

  let build_undirected_graph n_vertices f edges = 
    let e = Array.init n_vertices (fun _ -> []) in
    List.iter (fun c ->
      match f c with
      | None -> ()
      | Some(i1,i2) -> begin
        e.(i1) <- i2 :: e.(i1);
        e.(i2) <- i1 :: e.(i2)
      end) edges;
    fun vertex -> e.(vertex)

  let build_constraints (rules : rules) = 
    (* build rules over (new) state *)
    let state, rules = instantiate_rules rules in
    List.iter (show_rule_dr state) rules;
    (* get all pairs of rules *)
    let pairs = pairs rules in
    (* calculate pairwise relationships *)
    let constraints = 
      List.map 
        (fun ((n1,i1,r1),(n2,i2,r2)) ->
          let cf = conflict_free state r1 r2 in
          let me = mutually_exclusive state r1 r2 in
          let sc12 = sequentially_composable state r1 r2 in (* XXX if cf? *)
          let sc21 = sequentially_composable state r2 r1 in
          show_rule_constraints (n1,i1,r1) (n2,i2,r2) cf me sc12 sc21;
          {
            i1; i2; (* i1 < i2 *)
            cf; me; sc12; sc21;
          }) pairs
    in
    Array.of_list rules, constraints

  module INT = struct type t = int let compare = compare end
  module IntSet = Set.Make(INT)
  module IntMap = Map.Make(INT)

  let conflict_graph rules constraints = 
    let g = Array.fold_left (fun g (_,n,_) -> G.add_vertex g n) G.empty rules in
    let g = 
      let add_cf g c = if c.cf || c.me then g else G.add_edge g c.i1 c.i2 in
      List.fold_left add_cf g constraints 
    in
    gviz "conflict.dot" g;
    Connected.components_list g

  let enumerated_encoder rules group constraints =
    let rules = Array.of_list rules in
    let n_rules = Array.length rules in
    let asserted_bits i =
      let asserted j = if ((1 lsl j) land i) <> 0 then j else -1 in
      Array.init n_rules asserted
      |> Array.to_list |> List.filter ((<>)(-1))
    in
    Array.init (1 lsl n_rules) (fun i ->
      (* get bits asserted at this index *)
      let vertices = asserted_bits i in
      (*let out_edges = build_undirected_graph (List.length vertices) 
        (fun c -> 
      *)
      0)

(*
  module Graph = struct
    let neighbors a b = a
    let is_connected a b p = true
    let vertices _ = failwith ""
  end

  let fold_maximal_cliques f g accu =
    let rec extend clique cands nots accu =
      (* returns the accumulator ACCU plus all maximal cliques C that
      are supersets of CLIQUE, disjoint from NOTS, and subset of CANDS
      union CLIQUE.  *)
      let intersection_size a b = IntSet.cardinal (IntSet.inter a b) in
      if IntSet.is_empty cands then
        if IntSet.is_empty nots then f accu clique
        else accu
      else
        let pivot, _ =
          IntSet.fold
            (fun u (pivot, pivot_score) ->
              let score = intersection_size (Graph.neighbors g u) cands in
              if score > pivot_score then (u, score)
              else (pivot, pivot_score))
            (IntSet.union cands nots)
            (-1, -1) 
        in
        let _, _, accu =
          IntSet.fold
            (fun u ((cands, nots, accu) as state) ->
              if Graph.is_connected g u pivot then state
              else
                let cands = IntSet.remove u cands in
                let clique' = IntSet.add u clique in
                let u_neighbors = Graph.neighbors g u in
                let cands' = IntSet.inter cands u_neighbors in
                let nots' = IntSet.inter nots u_neighbors in
                let accu = extend clique' cands' nots' accu in
                let nots = IntSet.add u nots in
                  (cands, nots, accu))
            cands
            (cands, nots, accu)
        in
        accu
    in
    extend IntSet.empty (Graph.vertices g) IntSet.empty accu
*)
end

module Gcd = struct
          
  module S = interface x[8] y[8] end

  let na = S.(map (fun _ -> empty) t)

  let rules = 
    let open S in
    [
      "sub", (fun s -> {
        guard  = ((s.x >=: s.y) &: (s.y <>:. 0));
        action = { na with x = (s.x -: s.y) };
      });
          
      "swap", (fun s -> {
        guard  = ((s.x <: s.y) &: (s.y <>:. 0));
        action = { x = s.y; y = s.x };
      });
    ]

end

module Test = struct

  (* hand craft a few rules to test CF/SC *)

  module S = interface
    a[1] b[1] c[1] d[1]
  end
  let na = S.(map (fun _ -> empty) t)

  (* trivially CF and SC - rules do not access any shared state *)
  let rules0 = 
    let open S in
    [
      "a", (fun s -> {
        guard = s.a ==: s.b;
        action = { na with a = s.b; b = s.a };
      });
      "b", (fun s -> {
        guard = s.c ==: s.d;
        action = { na with c = s.d; d = s.c };
      });
    ]

  (* CF neither rules reads the state the other writes *)
  let rules1 = 
    let open S in
    [
      "a", (fun s -> {
        guard = s.a |: s.d;
        action = { na with a = s.c }
      });
      "b", (fun s -> {
        guard = s.b |: s.d;
        action = { na with b = s.c }
      });
    ]

  (* Not CF - both rules write state a, though neither reads it.
   * SC both ways *)
  let rules2 = 
    let open S in
    [
      "a", (fun s -> {
        guard = s.d;
        action = { na with a = s.c }
      });
      "b", (fun s -> {
        guard = s.b;
        action = { na with a = s.c }
      });
    ]

  (* SC a->b *)
  let rules3 = 
    let open S in
    [
      "a", (fun s -> { (* d=b,c,d r=a *)
        guard = s.d |: s.b;
        action = { na with a = s.c }
      });
      "b", (fun s -> { (* d=b,c,d r=a,b *)
        guard = s.b;
        action = { na with a = s.c; b = s.d }
      });
    ]

  (* SC b->a *)
  let rules4 = 
    let open S in
    [
      "a", (fun s -> {
        guard = s.b;
        action = { na with a = s.c; b = s.d }
      });
      "b", (fun s -> { 
        guard = s.d |: s.b;
        action = { na with a = s.c }
      });
    ]

end

module PartSched = struct


  module S = interface
    t{|6|}[1]
  end

  let cfp = [ (* pairs of conflict free rules *)
    1,2; 1,3; 1,5; 1,6;
    2,3; 2,4; 2,6;
    3,4; 3,5; 3,6;
    4,5;
    5,6
  ]
  let is_cf i j = 
    if i<j then List.mem (i,j) cfp
    else List.mem (j,i) cfp

  let rules = 
    let open S in
    let cf i = 
      fun (s : HardCaml.Signal.Comb.t S.t) -> { 
        guard = vdd;
        action = {
          t = Array.init 6 (fun j -> 
          if i=(j+1) then 
            reduce (|:) 
            (Array.to_list @@ Array.init 6 (fun j ->
              if is_cf i (j+1) then vdd else s.t.(j)))
          else empty)
        }
      }
    in
    [
      "t1", cf 1;
      "t2", cf 2;
      "t3", cf 3;
      "t4", cf 4;
      "t5", cf 5;
      "t6", cf 6;
    ]

end


