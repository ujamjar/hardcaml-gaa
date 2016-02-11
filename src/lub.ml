type sa = [ `c | `lr | `gr | `ext | `p | `l | `g | `cf | `me ]

type x = N of sa * x list
module S = Set.Make(struct type t = sa let compare = compare end)

let c   = N(`c  ,[])
let lr  = N(`lr ,[c])
let gr  = N(`gr ,[c])
let ext = N(`ext,[lr;gr])
let p   = N(`p  ,[c])
let l   = N(`l  ,[lr;p])
let g   = N(`g  ,[gr;p])
let cf  = N(`cf ,[l;g;ext])

let f = function
  | `c   -> c
  | `lr  -> lr
  | `gr  -> gr
  | `ext -> ext
  | `p   -> p
  | `l   -> l
  | `g   -> g
  | `cf  -> cf
  | `me  -> failwith "ME is not a valid scheduling annotation"

let idx = function
  | `cf  -> 0
  | `l   -> 1
  | `g   -> 2
  | `p   -> 3
  | `ext -> 4
  | `lr  -> 5
  | `gr  -> 6
  | `c   -> 7
  | `me  -> failwith "ME is not a valid scheduling annotation"

(* set of annots reachable from this annot *)
let rec reaches (N(x,l)) =
  S.add x 
    (List.fold_left S.union S.empty (List.map reaches l))

(* least upper bound between a and b is the minimum element in
    the intersection of the reachable annot's from a and b.

    note; that the annots are specified in a sorted(-ish) order so
          compare does the job *)
let lub a b = 
  let sa = reaches (f a) in
  let sb = reaches (f b) in
  let s = S.inter sa sb in
  List.hd @@ List.sort compare @@ S.elements s

(* pre-calculate the least upper bounds in a matrix *)
let annot : sa array = [| `cf; `l; `g; `p; `ext; `lr; `gr; `c |] 
let matrix = Array.map (fun a -> Array.map (fun b -> lub a b) annot) annot 

(* find lub by looking up matrix *)
let lub a b = matrix.(idx a).(idx b)

