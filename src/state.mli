module type S = sig
  type 'a t
  val length : 'a t -> int
  val of_list : (string * 'a) list -> 'a t
  val to_list : 'a t -> (string * 'a) list
  val iter : ((string * 'a) -> unit) -> 'a t -> unit
  val iter2 : ((string * 'a) -> (string * 'b) -> unit) -> 'a t -> 'b t -> unit
  val map : ((string * 'a) -> 'b) -> 'a t -> 'b list
  val map_t : ((string * 'a) -> (string * 'b)) -> 'a t -> 'b t
  val map2 : ((string * 'a) -> (string * 'b) -> 'c) -> 'a t -> 'b t -> 'c list
  val fold_left : ('a -> (string * 'b) -> 'a) -> 'a -> 'b t -> 'a
end

module State : S
module Inp : S
module Out : S
module Arg : S
module Ret : S

type state_spec = int State.t
type inp_spec = int Inp.t
type out_spec = int Out.t
type arg_spec = int Arg.t
type ret_spec = int Ret.t

open HardCaml.Signal

type state_sig = Comb.t State.t
type inp_sig = Comb.t Inp.t
type out_sig = Comb.t Out.t
type arg_sig = Comb.t Arg.t
type ret_sig = Comb.t Ret.t

