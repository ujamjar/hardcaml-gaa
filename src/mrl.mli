type module_defn
type module_instance
type read_method
type action_method
type variable = string
type rule
type exp
type prim_op
type action
type method_call

type scheduling_annotation = 
  | CF
  | ME
  | L
  | G
  | P
  | EXT
  | Lr
  | Gr
  | C

type scheduling_annotations = string array * scheduling_annotation array array

val (@?) : exp -> exp -> exp
val mux2 : exp -> exp -> exp -> exp
val const : int -> exp
val _false : exp
val _true : exp
val var : string -> exp
val ecall : string -> string -> exp list -> exp

val op2 : exp -> string -> exp -> exp
val (&:) : exp -> exp -> exp
val (|:) : exp -> exp -> exp
val (^:) : exp -> exp -> exp
val (~:) : exp -> exp

val ($?) : action list -> exp -> action
val _if : exp -> action list -> action list -> action
val acall : string -> string -> exp list -> action

val inst : string -> string -> module_instance
val rule : string -> ?guard:exp -> action list -> rule
val rmethod : string -> ?params:variable list -> ?guard:exp -> exp -> read_method
val amethod : string -> ?params:variable list -> ?guard:exp -> action list -> action_method
val _module : 
  ?insts:module_instance list -> 
  ?reads:read_method list ->
  ?actions:action_method list ->
  ?rules:rule list ->
  string -> module_defn

val when_lift_exp : exp -> exp
val when_lift_exps : exp list -> exp option * exp list
val when_lift_action : action -> action
val when_lift_actions : action list -> exp option * action list

val pretty : module_defn -> string

