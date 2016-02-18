open HardCaml.Signal.Types
open HardCaml.Signal.Comb

type sched_opt = [ `cf | `me | `sc ]

module StrSet = Set.Make(String)

let pairs l = 
  let rec pairs = function [] -> [] | h::t -> (List.map (fun t -> h,t) t) :: pairs t in
  List.concat (pairs l) 

module Make(T : Typ.S) = struct

  type state_map = (string * int * signal) T.S.t * t UidMap.t

  let empty_inter s0 s1 = UidSet.(is_empty @@ inter s0 s1) 

  (******************************************************************)
  (* domain and range of rules *)
  (******************************************************************)

  module DnR = struct

    type t = 
      {
        domain : UidSet.t Lazy.t;
        range : UidSet.t Lazy.t;
      }

    let rec domain_of_expr (x,state) set expr = 
      match UidMap.find (uid expr) state with
      | exception _ -> 
        List.fold_left (domain_of_expr (x,state)) set (deps expr)
      | _ -> 
        UidSet.add (uid expr) set

    let domain_of_guard state rule = domain_of_expr state UidSet.empty rule.Typ.guard

    let domain_of_action state rule = 
      List.fold_left (fun s e -> domain_of_expr state s e) UidSet.empty 
        (T.S.to_list rule.Typ.action)

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
        T.S.(to_list @@ map2 (fun (_,_,a) b -> a,b) state rule.Typ.action)

    let make state rules = 
      Array.of_list @@ List.map 
        (fun (_,r) -> 
          { 
            domain = lazy (domain state r);
            range = lazy (range state r); 
          }) rules

  end

  (******************************************************************)
  (* scheduling constraints *)
  (******************************************************************)

  module Constraints = struct

    open DnR

    type t = 
      {
        i1 : int;
        i2 : int;
        cf : bool Lazy.t;
        me : bool Lazy.t;
        sc12 : bool Lazy.t;
        sc21 : bool Lazy.t;
      }

    type fn = DnR.t -> DnR.t -> bool

    let conflict_free r1 r2 = 
      empty_inter (Lazy.force r1.domain) (Lazy.force r2.range) &&
      empty_inter (Lazy.force r2.domain) (Lazy.force r1.range) &&
      empty_inter (Lazy.force r1.range) (Lazy.force r2.range)

    let mutually_exclusive me_rules n1 n2 r1 r2 = 
      let is_user_me = 
        List.fold_left 
          (fun b me ->
            b || (StrSet.mem n1 me && StrSet.mem n2 me)) false me_rules
      in
      (* XXX scan top level conjunctions *)
      (* XXX check with sat solver *)
      is_user_me

    let sequentially_composable r1 r2 = 
      empty_inter (Lazy.force r2.domain) (Lazy.force r1.range)

    let make ~sched_opt ~me_rules dnr pairs =
      let me_rules = List.map StrSet.of_list me_rules in
      let calc_me, calc_cf, calc_sc = 
        List.mem `me sched_opt, List.mem `cf sched_opt, List.mem `sc sched_opt 
      in
      List.map 
        (fun ((n1,i1),(n2,i2)) ->
          let d1, d2 = dnr.(i1), dnr.(i2) in
          let me = lazy (calc_me && mutually_exclusive me_rules n1 n2 d1 d2) in
          let cf = lazy (Lazy.force me || (calc_cf && conflict_free d1 d2)) in
          (* ... me sat solver here ... *)
          let sc12 = lazy (Lazy.force cf || (calc_sc && sequentially_composable d1 d2)) in 
          let sc21 = lazy (Lazy.force cf || (calc_sc && sequentially_composable d2 d1)) in
          { i1; i2; cf; me; sc12; sc21; }) 
        pairs

    let is_parallel c = (Lazy.force c.cf) || (Lazy.force c.me) || (Lazy.force c.sc21)
    let not_parallel c = not (is_parallel c)

  end

  module Graph = struct

    open Constraints

    module type G = Graph.Sig.P  
      with type V.t = int
      and type V.label = int 
      and type E.t = int * int
      and type E.label = unit

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

    let connected g = Connected.components_list g
    let cliques g = Clique.maximalcliques g

    let gviz fname g = 
      let f = open_out fname in
      Gviz.output_graph f g;
      close_out f

    let build_graph ?show n_rules constraints f = 
      let g = (* add vertices *)
        let rec v g n = if n=n_rules then g else v (G.add_vertex g n) (n+1) in 
        v G.empty 0 
      in
      let g = List.fold_left (fun g c -> if f c then G.add_edge g c.i1 c.i2 else g) g constraints in
      (match show with None -> () | Some(fname) -> gviz fname g);
      g

    let conflict_graph n_rules constraints = 
      let g = build_graph ~show:"conflict.dot" n_rules constraints not_parallel in
      connected g |> List.map (List.sort compare)

  end

end

