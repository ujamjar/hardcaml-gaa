(********************************************************************************)
(* some dsl examples *)

(* check some early syntax *)
let m = Mrl.(_module "yo" 
  ~insts:[ 
    inst "mkFIFO" "fifo" 
  ] 
  ~rules:[
    rule "rrr" ~guard:_true [
      _if (_true) [] []
    ] 
  ] 
  ~reads:[
    rmethod "mmm" _false
  ]
  ~actions:[
    amethod "aaa" [
      [ _if (_true) [] [] ] $? _true
    ] 
  ]
)

let () = print_string @@ Mrl.pretty @@ m
let () = print_string @@ Mrl.pretty Prims.mkFIFO
