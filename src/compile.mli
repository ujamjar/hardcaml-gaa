module IntSet : Set.S with type elt = int

module Encoder : sig

  type t = [ `priority of int list | `enum of int list * int list array ]

  val enumerated_encoder : Rule.inst array -> Sched.Constraints.t list -> int list -> t

  open HardCaml.Signal.Comb

  val const_of_clique : int list -> int list -> t

  val pri_en : t list -> t list

  val pri_en_tree : t list -> t list

  val pri_mux : (t * t) list -> t * t

end

module Build : sig

  open HardCaml.Signal

  val state : st_spec:State.state_spec -> Sched.state_map

  val method_rules : 
    methods:(string * Method.meth) list -> i:State.inp_sig -> st:State.state_sig -> 
    (string * Rule.inst) list * (Comb.t * State.ret_sig) list

  val instantiate_rules : 
    methods:(string * Method.meth) list -> rules:(string * Rule.unrule) list ->
    i:State.inp_sig -> s:State.state_spec ->
    Sched.state_map * (string * Rule.inst) list * (Comb.t * State.ret_sig) list

  val rule_enables : 
    rules:(string * Rule.inst) array -> 
    sched:Encoder.t list -> 
    (int * Comb.t) list list

  val create_register_state :
    r_spec:Types.register -> 
    st_clear:State.state_sig -> 
    state:Sched.state_map -> 
    rules:(string * Rule.inst) array ->
    guards:(int * Comb.t) list list -> 
    Comb.t * State.state_sig

  val schedule : 
    ?sched_opt:Sched.sched_opt list -> ?me_rules:string list list ->
    methods:(string * Method.meth) list -> rules:(string * Rule.unrule) list ->
    i:State.inp_sig -> s:State.state_spec -> 
    Sched.state_map * 
    (string * Rule.inst) list * 
    (Comb.t * State.ret_sig) list * 
    Sched.Constraints.t list

  val print_constraints : 
    out_channel ->
    (string * Rule.inst) list ->
    Sched.Constraints.t list -> unit

  val print_schedule : 
    out_channel ->
    (string * Rule.inst) list ->
    int list list -> unit

  val compile : 
    ?sched_opt:Sched.sched_opt list -> ?me_rules:string list list ->
    r_spec:Types.register -> st_clear:State.state_sig -> 
    methods:(string * Method.meth) list -> rules:(string * Rule.unrule) list ->
    i:State.inp_sig -> s:State.state_spec -> 
    ((Comb.t * State.state_sig) * (Comb.t * State.ret_sig) list)

end

