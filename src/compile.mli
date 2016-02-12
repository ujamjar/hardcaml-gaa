module IntSet : Set.S with type elt = int

module Make(T : Typ.S) : sig
  
  open HardCaml.Signal.Types

  module Schedule : module type of Sched.Make(T)

  module Encoder : sig

    type t = [ `priority of int list | `enum of int list * int list array ]

    val enumerated_encoder : T.inst_rule array -> Schedule.Constraints.t list -> int list -> t

    open HardCaml.Signal.Comb

    val const_of_clique : int list -> int list -> t

    val pri_en : t list -> t list

    val pri_en_tree : t list -> t list

    val pri_mux : (t * t) list -> t * t

  end

  module Build : sig

    open HardCaml.Signal

    val state : unit -> Schedule.state_map

    val method_rules : 
      methods:(string * T.inst_meth) list -> i:signal T.I.t -> s:signal T.S.t -> 
      (string * T.inst_rule) list * (Comb.t * State.ret_sig) list

    val instantiate_rules : 
      methods:(string * T.inst_meth) list -> rules:(string * T.uninst_rule) list ->
      i:signal T.I.t -> 
      Schedule.state_map * (string * T.inst_rule) list * (Comb.t * State.ret_sig) list

    val rule_enables : 
      rules:(string * T.inst_rule) array -> 
      sched:Encoder.t list -> 
      (int * Comb.t) list list

    val create_register_state :
      r_spec:Types.register -> 
      st_clear:signal T.S.t -> 
      state:Schedule.state_map -> 
      rules:(string * T.inst_rule) array ->
      guards:(int * Comb.t) list list -> 
      Comb.t * signal T.S.t

    val schedule : 
      ?sched_opt:Sched.sched_opt list -> ?me_rules:string list list ->
      methods:(string * T.inst_meth) list -> rules:(string * T.uninst_rule) list ->
      i:signal T.I.t -> 
      Schedule.state_map * 
      (string * T.inst_rule) list * 
      (Comb.t * State.ret_sig) list * 
      Schedule.Constraints.t list

    val print_constraints : 
      out_channel ->
      (string * T.inst_rule) list ->
      Schedule.Constraints.t list -> unit

    val print_schedule : 
      out_channel ->
      (string * T.inst_rule) list ->
      int list list -> unit

    val compile : 
      ?sched_opt:Sched.sched_opt list -> ?me_rules:string list list ->
      r_spec:Types.register -> st_clear:signal T.S.t -> 
      methods:(string * T.inst_meth) list -> rules:(string * T.uninst_rule) list ->
      i:signal T.I.t -> 
      ((Comb.t * signal T.S.t) * (Comb.t * State.ret_sig) list)

  end

end

