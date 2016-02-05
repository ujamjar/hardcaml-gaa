type signal = 
  {
    name : string;
    signal : HardCaml.Signal.Comb.t;
  }

type spec = 
  {
    name : string;
    bits : int;
  }

module type S = sig
  type 'a t
  val of_list : (string * 'a) list -> 'a t
  val to_list : 'a t -> (string * 'a) list
  (*val map : ((string * 'a) -> 'b) -> 'a t -> 'a list*)
end

module Make( ) : S = struct

  type 'a t = (string * 'a) list

  let of_list l = l
  let to_list l = l

  (*let map f t = List.map f t*)

end

