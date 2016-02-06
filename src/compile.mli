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

  val state : st_spec:Rule.state_spec -> Sched.state_map

  val method_rules : 
    methods:(string * Rule.meth) list -> i:Rule.inp_sig -> st:Rule.state_sig -> 
    (string * Rule.inst) list * (Comb.t * Rule.ret_sig) list

  val instantiate_rules : 
    methods:(string * Rule.meth) list -> rules:(string * Rule.unrule) list ->
    i:Rule.inp_sig -> s:Rule.state_spec ->
    Sched.state_map * (string * Rule.inst) list * (Comb.t * Rule.ret_sig) list

  val rule_enables : 
    rules:(string * Rule.inst) array -> 
    sched:Encoder.t list -> 
    (int * Comb.t) list list

  val create_register_state :
    r_spec:Types.register -> 
    st_clear:Rule.state_sig -> 
    state:Sched.state_map -> 
    rules:(string * Rule.inst) array ->
    guards:(int * Comb.t) list list -> 
    Comb.t * Rule.state_sig

  val compile : 
    ?sched_opt:Sched.sched_opt list -> ?me_rules:string list list ->
    r_spec:Types.register -> st_clear:Rule.state_sig -> 
    methods:(string * Rule.meth) list -> rules:(string * Rule.unrule) list ->
    i:Rule.inp_sig -> s:Rule.state_spec -> 
    ((Comb.t * Rule.state_sig) * (Comb.t * Rule.ret_sig) list)

end

