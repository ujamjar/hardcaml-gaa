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
 * Scheduling
 * ==========
 *
 * In the reference system 1 enabled rules is selected
 * per step.  Where more than 1 rule is available to 
 * fire, a non-deterministic choice is made.
 *
 * For hardware I am considering a simpler model where
 * the user design specifies rule priorities and
 * the hardware is statically shceduled.
 *
 * 1] Rules will be provided in descending priority - 
 *    thas is the 1st enabled rule in the list will
 *    fire.  Any others that can fire in parallel will
 *    also be enabled.
 *
 * 2] The state update will be done in rule priority 
 *    order.
 *
 * 3] Pick only 1 of the SC rules ('<' or '>').  This
 *    avoids the complexity of removing cycles from the 
 *    directed constraint graph and allows us to fit 
 *    it in the strict rule order.  I think we must choose
 *    Ra > Rb (where Ra is less than Rb in the priority
 *    order).  Note that this will execute as 'Rb; Ra'.
 *    Debatably this is in the wrong order, however, it
 *    does mean the higher priority rule appears to
 *    produce the result.
 *
 * There seem to be a couple of nice properties that
 * reduce some of the complexity in the sceduler, but
 * also (possibly, we'll see) make it somewhat easier
 * to understand what rules will fire and when.
 *
 * 1] In the first step we will build the (undirected)
 *    constraint graph and find connected components.
 *    We would normally use CF+ME in this step, but I
 *    think the 1 sided SC rule could also be included.
 *    This will give a number of parallel scheduling 
 *    groups - 1 (or more) rules from each scheduling
 *    group can be enabled per cycle.
 *    (NEED TO CHECK THIS...)
 * 
 * 2] In the 2nd step we would either build a priority
 *    encoder (selects 1 valid rule per cycle) or an
 *    enumerated encoder.  In the latter case a (maximal) 
 *    clique is found among the groups constraints for each
 *    combination of enabled rules, and a rom is built.
 *    If we add the constraint that the clique used must
 *    include the highest priority enabled rule, we should
 *    respect the global ordering.
 *
 *)

open Printf
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

module Int = struct type t = int let compare = compare end
module IntSet = Set.Make(Int)

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

  let (--) s n = 
    let w = wire (width s) in
    w <== s;
    w -- n

  open HardCaml.Signal.Types

  type rule_u = string * (t S.t -> t S.t rule)
  type rule_i = string * int * t S.t rule
  type state = t S.t * t UidMap.t

  module StrSet = Set.Make(String)
  module StrMap = Map.Make(String)

  let state () = 
    let s = S.map (fun (_,b) -> wire b) S.t in
    let map = List.fold_left (fun m w -> UidMap.add (uid w) w m) UidMap.empty S.(to_list s) in
    s, map

  (* computation of domain and ranges.
   * XXX we recompute various domains for different functions.
   *     should really store them in a (lazy?) table *)

  let rec domain_of_expr (x,state) set expr = 
    match UidMap.find (uid expr) state with
    | exception _ -> 
      List.fold_left (domain_of_expr (x,state)) set (deps expr)
    | _ -> 
      UidSet.add (uid expr) set

  let domain_of_guard state rule = domain_of_expr state UidSet.empty rule.guard

  let domain_of_action state rule = 
    List.fold_left (domain_of_expr state) UidSet.empty S.(to_list rule.action)

  let domain state rule = 
    UidSet.union
      (domain_of_guard state rule)
      (domain_of_action state rule)

  let range (state,_) rule = 
    let add_to_range set (w,a) =
      if a = Signal_empty then set
      else UidSet.add (uid w) set
    in
    List.fold_left add_to_range UidSet.empty 
      S.(to_list @@ map2 (fun a b -> a,b) state rule.action)

  let empty_inter s0 s1 = UidSet.(is_empty @@ inter s0 s1) 

  let conflict_free state r1 r2 = 
    let domain_r1, domain_r2 = domain state r1, domain state r2 in
    let range_r1, range_r2 = range state r1, range state r2 in
    empty_inter domain_r1 range_r2 &&
    empty_inter domain_r2 range_r1 &&
    empty_inter range_r1 range_r2 

  let mutually_exclusive state r1 r2 = 
    (* XXX options
      * - use a sat solver!
      * - simple analysis based on conjunction of top level literals 
      * - user guided annotations *)
    false

  let sequentially_composable state r1 r2 = 
    empty_inter (domain state r2) (range state r1)

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

  let instantiate_rules rules =
    check_rule_names_unique rules;
    let state = state () in
    let rules = List.mapi (fun i (n,r) -> n, i, r (fst state)) rules in
    state, rules

  let print_uid_set s = 
    List.iter (printf "%Li ") (UidSet.elements s)

  let show_rule_dr st (n,i,r) = 
    printf "%s[%i] d={ " n i;
    print_uid_set (domain st r);
    printf "} r={ ";
    print_uid_set (range st r);
    printf "}\n"

  let show_rule_constraints (n1,i1,r1) (n2,i2,r2) cf me sc12 sc21 = 
    let b = function true -> "\xe2\x9c\x93" | false -> "\xe2\x9c\x97" in
    printf "%s[%i] <-> %s[%i] " n1 i1 n2 i2;
    printf "cf=%s me=%s sc12=%s sc21=%s\n" 
      (b cf) (b me) (b sc12) (b sc21)

  let pairs l = 
    let rec pairs = function [] -> [] | h::t -> (List.map (fun t -> h,t) t) :: pairs t in
    List.concat (pairs l) 

  let build_constraints rules =
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
    state, Array.of_list rules, constraints

  let is_cf_or_me c = c.cf || c.me
  let not_cf_or_me c = not (is_cf_or_me c)

  let build_graph ?show rules constraints f = 
    let g = Array.fold_left (fun g (_,n,_) -> G.add_vertex g n) G.empty rules in
    let g = List.fold_left (fun g c -> if f c then G.add_edge g c.i1 c.i2 else g) g constraints in
    (match show with None -> () | Some(fname) -> gviz fname g);
    g

  let conflict_graph rules constraints = 
    let g = build_graph ~show:"conflict.dot" rules constraints not_cf_or_me in
    Connected.components_list g |> List.map (List.sort compare)

  type encoder = [ `priority of int list | `enum of int list * int list array ]

  let asserted_bits size idx = 
    let rec f n = 
      if n=size then []
      else if (idx land (1 lsl n)) <> 0 then n :: f (n+1)
      else f (n+1)
    in
    f 0

  let max_clique g =
    (* find largest clique.  Break ties with lowest enabled index *)
    let m = Clique.maximalcliques g in
    let rec minl x = function [] -> x | h::t -> minl (min x h) t in 
    let m = List.map (fun m -> (List.length m, minl (-1) m), m) m in
    let cmp ((size1,min1),_) ((size2,min2),_) = 
      let c = - (compare size1 size2) in
      if c=0 then compare min1 min2 else c
    in
    let m = List.sort cmp m in
    assert (List.length m > 0);
    snd (List.hd m)

  let enumerated_encoder rules constraints group =
    (* get rules and constraints in group *)
    let get_group rules constraints group = 
      let rules = List.map (Array.get rules) group in
      let set = List.fold_left (fun s (_,i,_) -> IntSet.add i s) IntSet.empty rules in
      let constraints =
        let cond c = IntSet.mem c.i1 set && IntSet.mem c.i2 set && is_cf_or_me c in
        List.filter cond constraints 
      in
      Array.of_list rules, constraints
    in
    (* focus on this scheduling group *)
    let rules, constraints = get_group rules constraints group in 
    let n_rules = Array.length rules in
    if constraints = [] then `priority group
    else 
      (* find cliques *)
      `enum (group, 
        Array.init (1 lsl n_rules) 
          (fun i ->
            (* enabled subgroup *)
            let group = asserted_bits n_rules i in
            let rules, constraints = get_group rules constraints group in
            let g = build_graph rules constraints (fun _ -> true) in
            max_clique g))

  let const_of_clique gr cl = 
    let module B = HardCaml.Bits.Comb.IntbitsList in
    let size = List.length gr in
    constibl @@ 
      B.(reduce (|:) @@ 
        List.mapi (fun i idx -> if List.mem idx cl then sll (one size) i else zero size) gr)
   
  let pri_en = 
    let rec pri set = function
      | [] -> []
      | h::t -> 
        let b = h &: (~: set) in
        b :: pri b t
    in
    pri gnd

  let pri_en_tree x =  
    let rec pri1 = function 
      | [] -> [] 
      | [a] -> [[a],a] 
      | a::b::t -> 
        let a,b = a, (b &: (~: a)) in 
        ([a;b],(a|:b)) :: pri1 t 
    in
    let pri2 = function 
      | [a] -> a 
      | [(a,b);(c,d)] -> (a @ List.map (fun c -> c &: ~: b) c), b |: d 
      | _ -> failwith "bad pri tree"
    in
    try fst @@ tree 2 pri2 (pri1 x) with _ -> failwith "pri_en_tree"

  let rule_enables rules sched = 
    let get_enables gr = 
      List.map (fun i -> let n,_,r = Array.get rules i in r.guard -- ("__"^n^"_g")) gr 
    in
    List.map
      (fun s ->
        match s with
        | `priority gr -> List.map2 (fun idx en -> idx,en) gr (pri_en_tree @@ get_enables gr)
        | `enum (gr,cl) -> 
          let sel = concat @@ List.rev @@ get_enables gr in
          let data = List.map (const_of_clique gr) @@ Array.to_list cl in
          let en = List.rev @@ bits @@ mux sel data in
          List.map2 (fun idx en -> idx,en) gr en)
      sched

  let pri_mux l = 
    try
      tree 2 
        (function [e,a] -> e, a
                | [(e1,a1);(e2,a2)] -> e1 |: e2, mux2 e1 a1 a2
                | _ -> failwith "bad pri_mux tree") l
    with _ -> failwith "pri_mux"

  let sort_guards guards = 
    Array.of_list @@ List.sort (fun a b -> compare (fst a) (fst b)) @@ List.concat guards 

  let create_register_state r_spec st_clear (state,_) rules guards = 
    let guards = sort_guards guards in
    let rules = Array.init (Array.length rules) 
      (fun i -> 
        let n,j,r = rules.(i) in
        let k,e = guards.(i) in
        assert (i=j);
        assert (j=k);
        n, r, e -- ("__"^n^"_e"))
    in
    let any_enabled = reduce (|:) @@ Array.to_list @@ Array.map (fun (_,_,e) -> e) rules in

    let merge_st st a e = 
      S.(map2 (fun st a -> if a = Signal_empty then st else (e,a)::st) st a)
    in
    let st_mux = 
      Array.fold_left (fun st (n,r,e) -> merge_st st r.action e) 
        S.(map (fun _ -> []) t)
        rules
    in
    let st_reg = 
      let st = S.(map2 (fun s t -> s,t) st_mux st_clear) in
      S.(map2 
        (fun (n,b) (st,stc) -> 
          if st=[] then stc 
          else
            let e, a = pri_mux st in
            let r_spec = { r_spec with reg_clear_value=stc; reg_reset_value=stc } in
            HardCaml.Signal.Seq.(reg r_spec e a) -- n) t st) 
    in

    ignore @@ S.(map2 (<==) state st_reg);
    st_reg, any_enabled

  let compile_cf r_spec st_clear rules = 
    let state, rules, constraints = build_constraints rules in
    let scheduling_groups = conflict_graph rules constraints in
    let scheduling_encoders = List.map (enumerated_encoder rules constraints) scheduling_groups in
    let guards = rule_enables rules scheduling_encoders in
    create_register_state r_spec st_clear state rules guards 

  (* rules are enabled by an external guard and we return a ready signal *)
  let compile_cf_methods r_spec st_clear rules = 
    let state, rules, constraints = build_constraints rules in
    let scheduling_groups = conflict_graph rules constraints in
    let scheduling_encoders = List.map (enumerated_encoder rules constraints) scheduling_groups in
    let guards = rule_enables rules scheduling_encoders in
    create_register_state r_spec st_clear state rules guards 

end

module type Gaa = sig
  module I : HardCaml.Interface.S
  module O : HardCaml.Interface.S
  module S : HardCaml.Interface.S
  val r_spec : HardCaml.Signal.Types.register
  val clear : t I.t -> t S.t
  val output : t I.t -> t S.t -> t O.t
end

module Gaa(G : Gaa) = struct

  module Sched = Schedule(G.S)
  module I = G.I
  module O = G.O
  
  let running_name = "__gaa_running"

  let f_en rules i = 
    let s,en = Sched.compile_cf G.r_spec (G.clear i) (rules i) in
    G.output i s, output running_name en

  let f rules i = fst (f_en rules i)

  let circuit name rules = 
    let i = G.I.(map (fun (n,b) -> input n b) t) in
    let o = f rules i in
    let o = G.O.(to_list @@ map2 (fun (n,b) o -> output n o) t o) in
    HardCaml.Circuit.make name o

  let circuit_en name rules = 
    let i = G.I.(map (fun (n,b) -> input n b) t) in
    let o,en = f_en rules i in
    let o = G.O.(to_list @@ map2 (fun (n,b) o -> output n o) t o) in
    HardCaml.Circuit.make name (output running_name en :: o)

end

