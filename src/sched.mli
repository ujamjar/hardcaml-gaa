open HardCaml.Signal.Types
open HardCaml.Signal.Comb

type state_map = t Rule.state * t UidMap.t

type sched_opt = [ `cf | `me | `sc ]

module StrSet : Set.S with type elt = string

(* domain and range of a rule *)
module DnR : sig

  type t = 
    {
      domain : UidSet.t Lazy.t;
      range : UidSet.t Lazy.t;
    }

  val domain_of_expr : state_map -> UidSet.t -> HardCaml.Signal.Comb.t -> UidSet.t

  val domain_of_guard : state_map -> Rule.inst -> UidSet.t

  val domain_of_action : state_map -> Rule.inst -> UidSet.t

  val domain : state_map -> Rule.inst -> UidSet.t

  val range : state_map -> Rule.inst -> UidSet.t

  val make : state_map -> Rule.inst list -> t array

end

(* rule constraints *)
module Constraints : sig

  type t = 
    {
      i1 : int; (* rule indices *)
      i2 : int;
      cf : bool Lazy.t; (* constraints *)
      me : bool Lazy.t;
      sc12 : bool Lazy.t;
      sc21 : bool Lazy.t;
    }

  type fn = DnR.t -> DnR.t -> bool

  val conflict_free : fn

  val mutually_exclusive : StrSet.t list -> string -> string -> fn

  val sequentially_composable : fn

  val make : 
    sched_opt:sched_opt list -> me_rules:string list list -> 
    DnR.t array -> ((string*int) * (string*int)) list -> t list

  val is_parallel : t -> bool
  val not_parallel : t -> bool

end

module Graph : sig

  module type G = Graph.Sig.P  
    with type V.t = int
    and type V.label = int 
    and type E.t = int * int
	  and type E.label = unit

  module G : G
  module D : G

  val connected : G.t -> int list list
  val cliques : G.t -> int list list

  val gviz : string -> G.t -> unit

  val build_graph : ?show:string -> int -> Constraints.t list -> (Constraints.t -> bool) -> G.t

  val conflict_graph : int -> Constraints.t list -> int list list

end

