(*
Prototype of method calls for hardcaml-gaa, somewhat modelled on ocaml-ctypes.

A set of GAA modules can form a tree like hierarchy.  A module can communicate
with it's direct children though action and read methods.  These methods 
take and return parameters.  Somehow we need to model these methods and construct
appropriate hardware interfaces for them - thus we also need to be able to 
analyse the method itself.

Here we provide an API for describing, implementing and calling methods while
remaining reasonably well typed.  The implementation so far is for read methods
which have the following properties

- multiple input parameters
- multiple output values
- all parameters are the same type - `signal`
- each parameter is labelled with a string name and bit width

The shape of a method call is 

```
t -> t -> ... -> (t * t) * (t * (... * t))
```

Input parameters are passed in curried form, while return values are built
recursively from pairs (this could be extended quite easily upto, say,
5 arg tuples).

The following are a few example method calls described by the API
where `a,b,c,d` describe the names and widths of each parameter.

```
returning a : t
a @-> returning b : t -> t
a @-> b @-> returning (!c @ !d) : t -> t -> t * t
a @-> returning ((!b @ !c) @ !d) : t -> (t * t) * t
```

The following functions are provided to manipulate methods, and
can be used to describe simple hardcaml functions/circuits which 
fit the above form (avoiding the heavier weight interfaces/functor
approach).

- `define` create a method definiton
- `call` call a method (user supplied parameters)
- `inst` instantiate a method (auto-create inputs/outputs)
- `circuit` generate a circuit from the method

note; I have just realised that using an uncurried form for
input parameters might make this rather simpler....

*)

open HardCaml.Signal.Types

type param = string * int
type ienv = param -> signal
type oenv = param -> signal -> signal

module Return : sig
  
  type _ t = private
    | S : param -> signal t
    | P : 'a t * 'b t -> ('a * 'b) t

  val (!) : param -> signal t

  val (@) : 'a t -> 'b t -> ('a * 'b) t

  val map : oenv -> 'a t -> 'a -> 'a

  val list : 'a t -> 'a -> signal list

  val params : 'a t -> param list

end = struct

  type _ t = 
    | S : param -> signal t
    | P : 'a t * 'b t -> ('a * 'b) t

  let (!) s = S s
  
  let (@) a b = P(a,b)

  let rec map : type a. oenv -> a t -> a -> a = fun env f t -> 
    match f,t with
    | S s, a -> env s a
    | P(a,b), (c,d) -> map env a c, map env b d

  let rec list : type a. a t -> a -> signal list = fun f t ->
    match f,t with
    | S _, s -> [s]
    | P(a,b), (c,d) -> List.concat [ list a c; list b d ]

  let rec params : type a. a t -> param list = function
    | S s -> [s]
    | P(a,b)  -> List.concat [ params a; params b ]

end

module Func : sig

  type (_,_) t = private
    | R : 'a Return.t -> ('a,'a) t 
    | A : param * ('a,'b) t -> (signal -> 'a, 'b) t

  val ( @-> ) : param -> ('a, 'b) t -> (signal -> 'a, 'b) t 
  
  val returning : 'a Return.t -> ('a, 'a) t

  val get_return : ('a,'b) t -> 'b Return.t

end = struct

  type (_,_) t = 
    | R : 'a Return.t -> ('a,'a) t 
    | A : param * ('a,'b) t -> (signal -> 'a, 'b) t

  let (@->) a f = A(a, f)

  let returning p = R p

  let rec get_return : type a b. (a,b) t -> b Return.t = function
    | R p -> p
    | A(a,b) -> get_return b

end

module Rmethod : sig
  
  exception Parameter_validation of string * int * int

  val (!) : param -> signal Return.t

  val (@) : 'a Return.t -> 'b Return.t -> ('a * 'b) Return.t

  val ( @-> ) : param -> ('a, 'b) Func.t -> (signal -> 'a, 'b) Func.t 
  
  val returning : 'a Return.t -> ('a, 'a) Func.t

  type ('a,'b) defn = ('a,'b) Func.t * 'a

  val define : ('a,'b) Func.t -> 'a -> ('a,'b) defn 

  val call : ('a,'b) defn -> 'a 

  type inst_env = 
    {
      input : ienv;
      output : oenv;
    }

  val inst : inst_env -> ('a,'b) defn -> 'b 

  val returns : ('a,'b) defn -> 'b -> signal list

  val ioenv : inst_env

  val circuit : string -> ('a,'b) defn -> HardCaml.Circuit.t

end = struct

  exception Parameter_validation of string * int * int

  let (!) = Return.(!)
  let (@) = Return.(@)
  let (@->) = Func.(@->)
  let returning = Func.returning

  type ('a,'b) defn = ('a,'b) Func.t * 'a

  let define : type a b. (a,b) Func.t -> a -> (a,b) defn = fun t f -> t,f

  (* this could be [let call (_, f) -> f].
     however, with this we can have parameter validation *)
  let rec call : type a b. (a,b) defn -> a = fun (t, f) ->
    let valid (n,b) s = if width s <> b then raise (Parameter_validation(n, b, width s)) in
    match t with
    | Func.R p -> Return.map (fun p s -> valid p s; s) p f
    | Func.A(a,b) -> (fun p -> valid a p; call (b,(f p)))

  type inst_env = 
    {
      input : ienv;
      output : oenv;
    }

  let rec inst : type a b. inst_env -> (a,b) defn -> b = fun env (t,f) ->
    match t with
    | Func.R p -> Return.map env.output p f
    | Func.A(a,b) -> inst env (b, (f (env.input a))) 

  let returns (t,f) r = Return.list (Func.get_return t) r

  let ioenv = 
    {
      input = (fun (n,b) -> HardCaml.Signal.Comb.input n b);
      output = (fun (n,b) s -> HardCaml.Signal.Comb.output n s);
    }

  let circuit name defn = 
    HardCaml.Circuit.make name 
      (returns defn (inst ioenv defn))

end

module Example = struct

  (* examples with various shapes *)

  open HardCaml.Signal.Comb
  open Rmethod

  let a, b, c, d = ("a",8),("b",8),("c",8),("d",8)

  (* t *)
  let const_typ = returning !a 

  let const_defn = define const_typ (consti 8 254)

  let const_call = call const_defn 

  let const_inst = inst ioenv const_defn 

  let const_circ = circuit "const" const_defn

  (* t -> t *)
  let not_typ = b @-> returning !c 

  let not_defn = define not_typ (fun a -> ~: a)

  let not_call = call not_defn (input "b" 8) 

  let not_inst = inst ioenv not_defn 

  let not_circ = circuit "not" not_defn

  (* t -> t -> t *)
  let add_typ = a @-> b @-> returning !c 

  let add_defn = define add_typ (fun a b -> a +: b)

  let add_call = call add_defn (input "a" 8) (input "b" 8)

  let add_inst = inst ioenv add_defn 

  let add_circ = circuit "add" add_defn

  (* t -> t -> t * t *)
  let addsub_typ = a @-> b @-> returning (!c @ !d) 

  let addsub_defn = define addsub_typ (fun a b -> a +: b, a -: b)

  let addsub_call = call addsub_defn (input "a" 8) (input "b" 8)

  let addsub_inst = inst ioenv addsub_defn 

  let addsub_circ = circuit "addsub" addsub_defn

  (* t -> t * (t * t) *)
  let tup3_typ = a @-> returning (!b @ !c @ !d) 

  let tup3_defn = define tup3_typ (fun a -> a +:. 1, (a +:. 2, a +:. 3))

  let tup3_call = call tup3_defn (input "a" 8) 

  let tup3_inst = inst ioenv tup3_defn 

  let tup3_circ = circuit "tup3" tup3_defn

end

