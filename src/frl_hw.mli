open HardCaml.Signal.Comb
open HardCaml.Signal.Types

type 'a rule = 
  {
    guard : t;
    action : 'a;
  }

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

type sched_opt = [ `cf | `me | `sc ]

module Schedule(I : HardCaml.Interface.S)(S : HardCaml.Interface.S) : sig

  (* uninstantiated rule *)
  type rule_u = string * (t I.t -> t S.t -> t S.t rule)

  (* method rule *)
  type rule_m = string * (t * t list * (string*int) list * 
                          (t I.t -> t S.t -> t list -> t S.t rule * t list))

  (* instantiated rule *)
  type rule_i = string * int * t S.t rule

  type state = t S.t * t UidMap.t

  module StrSet : Set.S

  val state : unit -> state

  val domain_of_expr : state -> UidSet.t -> t -> UidSet.t

  val domain_of_guard : state -> 'a rule -> UidSet.t

  val domain_of_action : state -> t S.t rule -> UidSet.t

  val domain : state -> t S.t rule -> UidSet.t

  val range : state -> t S.t rule -> UidSet.t

  val conflict_free : state -> t S.t rule -> t S.t rule -> bool

  val mutually_exclusive : StrSet.t list -> state -> 
    string * t S.t rule -> string * t S.t rule -> bool

  val sequentially_composable : state -> t S.t rule -> t S.t rule -> bool

  val instantiate_rules : rule_m list -> rule_u list -> t I.t -> state * rule_i list * (t * t list) list

  val build_constraints : 
    sched_opt:sched_opt list ->
    me_rules:string list list ->
    state -> rule_i list ->
    rule_i array * constraints list

  val conflict_graph : rule_i array -> constraints list -> int list list

  val asserted_bits : int -> int -> int list

  type encoder = [ `priority of int list | `enum of int list * int list array ]

  val enumerated_encoder : rule_i array -> constraints list -> int list -> encoder 

  val compile : 
    ?sched_opt:sched_opt list ->
    ?me_rules:string list list ->
    r_spec:register -> st_clear:t S.t -> 
    methods:rule_m list -> rules:rule_u list -> i:t I.t ->
    (t S.t * t) * (t * t list) list

end

module type Gaa = sig
  module I : HardCaml.Interface.S
  module O : HardCaml.Interface.S
  module S : HardCaml.Interface.S
  val name : string
  val methods : 
    (string * (string*int) list * (string*int) list * 
    (t I.t -> t S.t -> t list -> t S.t rule * t list)) list
  val rules : (string * (t I.t -> t S.t -> t S.t rule)) list
  val r_spec : HardCaml.Signal.Types.register
  val clear : t I.t -> t S.t
  val output : t I.t -> t S.t -> t O.t
  val sched_opt : sched_opt list
  val me_rules : string list list
end

module Gaa(G : Gaa) : sig
  module Sched : module type of Schedule(G.I)(G.S)

  val n_methods : int

  module I : interface
    (i : G.I)
    method_en{ }
    method_in{ }
  end
  module O : interface
    (o : G.O)
    method_vld{ }
    method_out{ }
    rules_running
  end

  val f : t I.t -> t O.t

  val circuit : string -> HardCaml.Circuit.t

end

