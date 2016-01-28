(* some extended functionality for graphs. *)

module Components = struct

  (* copied from ocamlgraph.  not in 1.8.6 *)

  open Graph

  module type U = sig
    type t
    module V : Sig.COMPARABLE
    val iter_vertex : (V.t -> unit) -> t -> unit
    val iter_edges : (V.t -> V.t -> unit) -> t -> unit
  end

  module Undirected(G: U) = struct

    module UF = Unionfind.Make(G.V)
    module H = Hashtbl.Make(G.V)

    let components g =
      let vertices = ref [] in
      G.iter_vertex (fun v -> vertices := v :: !vertices) g;
      let uf = UF.init !vertices in
      let visit u v = UF.union u v uf in
      G.iter_edges visit g;
      let count = ref 0 in
      let comp = H.create 5003 in
      let visit v =
        let v = UF.find v uf in
        if not (H.mem comp v) then begin H.add comp v !count; incr count end in
      G.iter_vertex visit g;
      !count, (fun v -> H.find comp (UF.find v uf))

    let components_array g =
      let n,f = components g in
      let t = Array.make n [] in
      G.iter_vertex (fun v -> let i = f v in t.(i) <- v :: t.(i)) g;
      t

    let components_list g =
      let a = components_array g in
      Array.fold_right (fun l acc -> l :: acc) a []

  end

end

module Feedback_arc_set = struct

  (* Minimum feedback arc set approximation for directed graphs *)

  open Graph

  module Make(G : Graph.Sig.G) = struct

    type v = 
      {
        mutable indeg : int;
        mutable outdeg : int;
        mutable idx : int;
        mutable order : int;
        v : G.V.t;
      }
    
    module H = Hashtbl.Make(G.V)

    (* abstract out the queue structure so we can make it match the reference version,
     * if needed *)
    module Q : sig
      type t = v
      type q
      val empty : unit -> q
      val is_empty : q -> bool
      val pop : q -> t
      val push : q -> t -> unit
    end = struct

      type t = v
      type q = v list ref

      let empty () = ref []
      let is_empty q = !q = []

      let pop s = 
        let h,t = List.hd !s, List.tl !s in
        s := t;
        h

      let push s v = s := v :: !s 

    end

    let eades g = 

      let n_vertices = G.nb_vertex g in
      let idx, vertices = ref 0, ref [] in
      let vhash = H.create 5003 in
      let mk v = 
        incr idx;
        H.add vhash v (!idx - 1);
        { indeg = G.in_degree g v; outdeg = G.out_degree g v; 
          idx = !idx - 1; order = 0; v } 
      in
      G.iter_vertex (fun v -> vertices := mk v :: !vertices) g;
      let vertices = Array.of_list !vertices in

      let order_next_pos = 
        let pos = ref 0 in
        fun v -> v.order <- !pos; incr pos; v.indeg <- -1; v.outdeg <- -1;
      in
      let order_next_neg = 
        let neg = ref 0 in
        fun v -> v.order <- !neg; decr neg; v.indeg <- -1; v.outdeg <- -1;
      in

      let nodes_left = ref n_vertices in
      let sources, sinks = Q.empty(), Q.empty() in

      Array.iter 
        (fun v ->
          if v.indeg = 0 then 
            if v.outdeg = 0 then begin 
              decr nodes_left;
              order_next_pos v 
            end else 
              Q.push sources v 
          else 
            Q.push sinks v) vertices;

      let remove_outgoing_edges v = 
        G.iter_succ 
          (fun v -> 
            let v = vertices.(H.find vhash v) in
            if v.indeg > 0 then begin
              v.indeg <- v.indeg - 1;
              (if v.indeg = 0 then Q.push sources v)
            end) g v.v
      in

      let remove_incoming_edges v = 
        G.iter_pred
          (fun v -> 
            let v = vertices.(H.find vhash v) in
            if v.outdeg > 0 then begin
              v.outdeg <- v.outdeg - 1;
              (if v.outdeg = 0 && v.indeg > 0 then Q.push sinks v)
            end) g v.v
      in

      let rec remove_sources () = 
        if not (Q.is_empty sources) then begin
          let v = Q.pop sources in
          order_next_pos v;
          remove_outgoing_edges v;
          decr nodes_left;
          remove_sources ()
        end
      in

      let rec remove_sinks () = 
        if not (Q.is_empty sinks) then begin
          let v = Q.pop sinks in
          order_next_neg v;
          remove_incoming_edges v;
          decr nodes_left;
          remove_sinks ()
        end
      in

      let choose_vertex () = 
        fst @@ Array.fold_left 
          (fun (v,best) v' ->
            if v'.outdeg >= 0 then 
              let diff = v'.outdeg - v'.indeg in
              if diff > best || v=None then (Some(v'), diff)
              else (v,best)
            else (v,best))
          (None,0) vertices
      in

      let rec while_not_empty () = 
        if !nodes_left > 0 then begin
          remove_sources ();
          remove_sinks ();
          (match choose_vertex () with
          | None -> ()
          | Some(v) -> begin
            order_next_pos v;
            remove_outgoing_edges v;
            remove_incoming_edges v;
            decr nodes_left;
          end);
          while_not_empty ()
        end
      in

      while_not_empty ();

      Array.init n_vertices 
        (fun i -> 
          let ord = vertices.(i).order in
          let ord = if ord < 0 then ord + n_vertices else ord in
          vertices.(ord).v)

  end

end

module F = Feedback_arc_set.Make(Graph.Pack.Digraph)

