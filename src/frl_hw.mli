open HardCaml.Signal.Comb
open HardCaml.Signal.Types

type 'a rule = 
  {
    guard : t;
    action : 'a;
  }

module Schedule(S : HardCaml.Interface.S) : sig

  (* uninstantiated rule *)
  type rule_u = string * (t S.t -> t S.t rule)

  (* instantiated rule *)
  type rule_i = string * int * t S.t rule

  type state = t S.t * t UidMap.t

  val state : unit -> state

  val domain_of_expr : state -> UidSet.t -> t -> UidSet.t

  val domain_of_guard : state -> 'a rule -> UidSet.t

  val domain_of_action : state -> t S.t rule -> UidSet.t

  val domain : state -> t S.t rule -> UidSet.t

  val range : state -> t S.t rule -> UidSet.t

  val conflict_free : state -> t S.t rule -> t S.t rule -> bool

  val mutually_exclusive : state -> t S.t rule -> t S.t rule -> bool

  val sequentially_composable : state -> t S.t rule -> t S.t rule -> bool

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

  val instantiate_rules : rule_u list -> state * rule_i list

  val build_constraints : rule_u list -> state * rule_i array * constraints list

  val conflict_graph : rule_i array -> constraints list -> int list list

  val asserted_bits : int -> int -> int list

  type encoder = [ `priority of int list | `enum of int list * int list array ]

  val enumerated_encoder : rule_i array -> constraints list -> int list -> encoder 

  val compile_cf : register -> t S.t -> rule_u list -> t S.t * t

end

module type Gaa = sig
  module I : HardCaml.Interface.S
  module O : HardCaml.Interface.S
  module S : HardCaml.Interface.S
  val r_spec : HardCaml.Signal.Types.register
  val clear : t I.t -> t S.t
  val output : t I.t -> t S.t -> t O.t
end

module Gaa(G : Gaa) : sig
  module Sched : module type of Schedule(G.S)
  module I : HardCaml.Interface.S
  module O : HardCaml.Interface.S

  val f_en : (t G.I.t -> Sched.rule_u list) -> t G.I.t -> t G.O.t * t
  val circuit_en : string -> (t G.I.t -> Sched.rule_u list) -> HardCaml.Circuit.t

  val f : (t G.I.t -> Sched.rule_u list) -> t G.I.t -> t G.O.t
  val circuit : string -> (t G.I.t -> Sched.rule_u list) -> HardCaml.Circuit.t
end

