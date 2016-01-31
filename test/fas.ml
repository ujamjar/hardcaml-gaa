(* tests for the feedback arc set algorithm *)
open Printf

module G = Graph.Persistent.Digraph.Concrete(struct 
  type t = int 
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end)
module F = HardCamlGaa.Graph_ex.Feedback_arc_set.Make(G)

let g0 = [ 0,1; 1,2; 2,0; 2,3; 2,4; 0,4; 4,3; 5,0; 6,5; ]
let g1 = [ 0,1; 1,2; 2,0; 2,3; 2,4; 0,4; 4,3; 5,0; 6,5; 1,1; 4,4; ]

let mk_graph = List.fold_left (fun g (e,f) -> G.add_edge g e f) G.empty

let eades g = 
  let fas = F.eades (mk_graph g) in
  Array.iter (printf "%i ") fas;
  printf "\n"

let () = eades g0
let () = eades g1

