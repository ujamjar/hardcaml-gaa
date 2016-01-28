(* FRL types *)
module Types : sig

  type module_defn
  type primitive
  type exp
  type rule
  type prim_op
  type action
  type method_call

end

(* DSL *)
module Dsl : sig

  open Types

  val mux2 : exp -> exp -> exp -> exp
  val const : int -> exp
  val _false : exp
  val _true : exp
  val ecall : string -> string -> exp list -> exp

  val op2 : exp -> string -> exp -> exp
  val (&:) : exp -> exp -> exp
  val (|:) : exp -> exp -> exp
  val (^:) : exp -> exp -> exp
  val (~:) : exp -> exp

  val (+:) : exp -> exp -> exp
  val (-:) : exp -> exp -> exp

  val (<:) : exp -> exp -> exp
  val (<=:) : exp -> exp -> exp
  val (>:) : exp -> exp -> exp
  val (>=:) : exp -> exp -> exp
  val (==:) : exp -> exp -> exp
  val (<>:) : exp -> exp -> exp

  val _if : exp -> action list -> action list -> action
  val acall : string -> string -> exp list -> action

  val reg : string -> < get : exp; set : exp -> action; prim : primitive >

  val rule : string -> ?guard:exp -> action list -> rule
  val _module :
    prims:primitive list ->
    rules:rule list ->
    string -> module_defn

end

(* pretty printer *)
module Pretty : sig
  val to_string : Types.module_defn -> string
end

(* simple TRS evaluation *)
module Eval : sig
  val run : ?steps:int -> Types.module_defn -> (string * int) list
end

(* XXX test circuit *)
module Gcd : sig
  val gcd : Types.module_defn
end

