open HardCaml.Signal.Types

type 'a rule = 
  {
    guard : signal;
    action : 'a;
  }

module type S = sig

  module I : HardCaml.Interface.S 
  module S : HardCaml.Interface.S 
  module O : HardCaml.Interface.S 

  (* rules *)

  type uninst_rule = i:signal I.t -> s:signal S.t -> (signal S.t rule)

  type inst_rule = signal S.t rule

  type uninst_meth = {
    arg_spec : State.arg_spec;
    ret_spec : State.ret_spec;
    fn : i:signal I.t -> s:signal S.t -> a:State.arg_sig -> (inst_rule * State.ret_sig);
  }

  type inst_meth = {
    en : signal;
    args : State.arg_sig;
    spec : uninst_meth;
  }

  type param = string * int
  type ienv = param -> signal
  type oenv = param -> signal -> signal

  module Return : sig
    
    type _ t = 
      | S : param -> signal t
      | P : 'a t * 'b t -> ('a * 'b) t

    val (!) : param -> signal t

    val (@) : 'a t -> 'b t -> ('a * 'b) t

    val map : oenv -> 'a t -> 'a -> 'a

    val mk : ienv -> 'a t -> 'a

    val list : 'a t -> 'a -> signal list

    val params : 'a t -> param list

  end 

  module Func(R : sig type 'a t end) : sig

    type (_,_) t = 
      | R : 'a R.t -> ('a,'a) t 
      | A : param * ('a,'b) t -> (signal -> 'a, 'b) t

    val ( @-> ) : param -> ('a, 'b) t -> (signal -> 'a, 'b) t 
    
    val returning : 'a R.t -> ('a, 'a) t

    val get_return : ('a,'b) t -> 'b R.t

  end 
(*
  module Rmethod : sig
    
    module Func : module type of Func(Return)

    exception Parameter_validation_error of string * string * int * int

    val (!) : param -> signal Return.t

    val (@) : 'a Return.t -> 'b Return.t -> ('a * 'b) Return.t

    val ( @-> ) : param -> ('a, 'b) Func.t -> (signal -> 'a, 'b) Func.t 
    
    val returning : 'a Return.t -> ('a, 'a) Func.t

    type 'a fn = i:signal I.t -> s:signal S.t -> 'a

    type ('a,'b) defn = string * ('a,'b) Func.t * 'a fn

    val define : string -> ('a,'b) Func.t -> 'a fn -> ('a,'b) defn 

    val call : ('a,'b) defn -> 'a fn
(*
    type inst_env = 
      {
        input : ienv;
        output : oenv;
      }

    val inst : inst_env -> ('a,'b) defn -> 'b 

    val returns : ('a,'b) defn -> 'b -> signal list
*)

  end 
*)
  module Rmethod2 : sig
    
    module Func : module type of Func(Return)

    exception Parameter_validation_error of string * string * int * int

    val (!) : param -> signal Return.t

    val (@) : 'a Return.t -> 'b Return.t -> ('a * 'b) Return.t

    val ( @-> ) : param -> ('a, 'b) Func.t -> (signal -> 'a, 'b) Func.t 
    
    val returning : 'a Return.t -> ('a, 'a) Func.t

    type 'a fn = i:signal I.t -> s:signal S.t -> 'a rule

    type ('a,'b) defn = string * ('a,'b) Func.t * 'a fn

    val define : string -> ('a,'b) Func.t -> 'a fn -> ('a,'b) defn 

    (*val call : ('a,'b) defn -> 'a fn*)

    type rinst = ((string * int) * signal) list

    val params : ('a,'b) defn -> param list * param list

    val inst : i:signal I.t -> s:signal S.t -> ('a,'b) defn -> (rinst * rinst) rule

    val call2 : ('a,'b) defn -> 'a 

  end 

  module Amethod : sig

    module R : sig
      type defn = signal S.t
      type inst = (param * signal) list
      type _ t = 
        | D : defn t
        | C : inst t 
    end

    module Func : module type of Func(R)

    type ('a,'b,'c,'d) func = ('a,'b) Func.t * ('c,'d) Func.t

    val ( @-> ) : param -> ('a,'b,'c,'d) func -> (signal->'a,'b,signal->'c,'d) func
    
    val returning : unit -> (R.defn, R.defn, R.inst, R.inst) func

    type 'a fn = i:signal I.t -> s:signal S.t -> 'a rule

    type ('a,'b,'c,'d) defn = string * ('a,'b,'c,'d) func * 'a fn

    val define : string -> ('a,'b,'c,'d) func -> 'a fn -> ('a,'b,'c,'d) defn

    val params : ('a,'b,'c,'d) defn -> param list 

    val inst : i:signal I.t -> s:signal S.t -> ('a,R.defn,'c,'d) defn -> (R.inst * R.defn) rule

    val call : ('a,'b,'c,R.inst) defn -> 'c

  end

end

module Make(I : HardCaml.Interface.S)
           (S : HardCaml.Interface.S) 
           (O : HardCaml.Interface.S) = struct

  open HardCaml.Signal.Types

  module I = I
  module S = S
  module O = O

  type uninst_rule = i:signal I.t -> s:signal S.t -> (signal S.t rule)

  type inst_rule = signal S.t rule

  type uninst_meth = {
    arg_spec : State.arg_spec;
    ret_spec : State.ret_spec;
    fn : i:signal I.t -> s:signal S.t -> a:State.arg_sig -> (inst_rule * State.ret_sig);
  }

  type inst_meth = {
    en : signal;
    args : State.arg_sig;
    spec : uninst_meth;
  }

  type param = string * int
  type ienv = param -> signal
  type oenv = param -> signal -> signal

  module Return = struct

    type _ t = 
      | S : param -> signal t
      | P : 'a t * 'b t -> ('a * 'b) t

    let (!) s = S s
    
    let (@) a b = P(a,b)

    let rec map : type a. oenv -> a t -> a -> a = fun env f t -> 
      match f,t with
      | S s, a -> env s a
      | P(a,b), (c,d) -> map env a c, map env b d

    let rec mk : type a. ienv -> a t -> a = fun env f ->
      match f with
      | S s -> env s
      | P(a,b) -> mk env a, mk env b

    let rec list : type a. a t -> a -> signal list = fun f t ->
      match f,t with
      | S _, s -> [s]
      | P(a,b), (c,d) -> List.concat [ list a c; list b d ]

    let rec params : type a. a t -> param list = function
      | S s -> [s]
      | P(a,b)  -> List.concat [ params a; params b ]

  end

  module Func(R : sig type 'a t end) = struct

    type (_,_) t = 
      | R : 'a R.t -> ('a,'a) t 
      | A : param * ('a,'b) t -> (signal -> 'a, 'b) t

    let (@->) a f = A(a, f)

    let returning p = R p

    let rec get_return : type a b. (a,b) t -> b R.t = function
      | R p -> p
      | A(a,b) -> get_return b

  end
(*
  module Rmethod = struct

    module Func = Func(Return)

    exception Parameter_validation_error of string * string * int * int

    let (!) = Return.(!)
    let (@) = Return.(@)
    let (@->) = Func.(@->)
    let returning = Func.returning

    type 'a fn = i:signal I.t -> s:signal S.t -> 'a

    type ('a,'b) defn = string * ('a,'b) Func.t * 'a fn

    let define : type a b. string -> (a,b) Func.t -> a fn -> (a,b) defn = fun n t f -> n,t,f

    let call : type a b. (a,b) defn -> a fn = fun (fname,t,f) ~i ~s -> 
      let rec call : type a b. (a,b) Func.t * a -> a = fun (t,f) ->
        let valid (n,b) s = 
          if width s <> b then raise (Parameter_validation_error(fname, n, b, width s)) 
        in
        match t with
        | Func.R p -> Return.map (fun p s -> valid p s; s) p f
        | Func.A(a,b) -> (fun p -> valid a p; call (b,(f p)))
      in
      call (t, f ~i ~s)

(*
    type inst_env = 
      {
        input : ienv;
        output : oenv;
      }

    let rec inst : type a b. inst_env -> (a,b) defn -> b = fun env (t,f) ->
      match t with
      | Func.R p -> Return.map env.output p f
      | Func.A(a,b) -> inst env (b, (f (env.input a))) 
*)
    let returns (t,f) r = Return.list (Func.get_return t) r

  end
*)
  module Rmethod2 = struct

    module Func = Func(Return)

    exception Parameter_validation_error of string * string * int * int

    let (!) = Return.(!)
    let (@) = Return.(@)
    let (@->) = Func.(@->)
    let returning = Func.returning

    type 'a fn = i:signal I.t -> s:signal S.t -> 'a rule

    type ('a,'b) defn = string * ('a,'b) Func.t * 'a fn

    let define : type a b. string -> (a,b) Func.t -> a fn -> (a,b) defn = fun n t f -> n,t,f

    (* ... no, not right ... *)
    (*let call : type a b. (a,b) defn -> a fn = fun (fname,t,f) ~i ~s -> 
      let rec call : type a b. (a,b) Func.t * a -> a = fun (t,f) ->
        let valid (n,b) s = 
          if width s <> b then raise (Parameter_validation_error(fname, n, b, width s)) 
        in
        match t with
        | Func.R p -> Return.map (fun p s -> valid p s; s) p f
        | Func.A(a,b) -> (fun p -> valid a p; call (b,(f p)))
      in
      let f = f ~i ~s in
      { f with action = call (t,f.action) } *)

    let returns t r = Return.list (Func.get_return t) r

    type rinst = ((string * int) * signal) list

    let rec params : type a b. (a,b) Func.t -> param list -> param list * param list = fun t l ->
      match t with
      | Func.R p -> List.rev l, Return.params p
      | Func.A(a, b) -> params b (a::l)
    let params (_,t,_) = params t []

    (* instantiate rule, apply wires to inputs, collect outputs and return them *)
    let rec inst : type a b. (a,b) Func.t * a -> rinst -> rinst * rinst = fun (t,f) l ->
      match t with
      | Func.R p -> 
        let o = List.map2 (fun a b -> a,b) (Return.params p) (Return.list p f) in
        List.rev l, o
      | Func.A(a,b) ->
        let w = HardCaml.Signal.Comb.wire (snd a) in
        inst (b, (f w)) ((a,w)::l)
    let inst ~i ~s (n,t,f) = 
      let f = f ~i ~s in
      { f with action = inst (t,f.action) [] }

    (* nearly, but not quite ... 
     * call should return the output tuple... YES (TODO look up)!
     * it should also somehow record the input parameters... NO! *)
    let rec call2 : type a b. (a,b) Func.t -> rinst -> a = fun t l ->
      match t with
      | Func.R p -> Return.mk (fun p -> Signal_empty) p 
      | Func.A(a,b) -> (fun p -> call2 b ((a,p)::l))
    let call2 (n,t,f) = call2 t []

  end

  module Amethod = struct

    module R = struct
      type defn = signal S.t
      type inst = (param * signal) list
      type _ t = 
        | D : defn t
        | C : inst t 
    end

    module Func = Func(R)

    type ('a,'b,'c,'d) func = ('a,'b) Func.t * ('c,'d) Func.t

    let (@->) p (f0,f1) = Func.(p @-> f0, p @-> f1)

    let returning () = Func.returning R.D, Func.returning R.C

    type 'a fn = i:signal I.t -> s:signal S.t -> 'a rule

    type ('a,'b,'c,'d) defn = string * ('a,'b,'c,'d) func * 'a fn

    let define : type a b c d. string -> (a,b,c,d) func -> a fn -> (a,b,c,d) defn = fun n t f -> n,t,f

    (* this evaluates the method call, returning the state update function - not useful? *)
    (*let rec eval : type a b. (a,b) Func.t * a -> a = fun (t,f) ->
      match t with
      | Func.R p -> f
      | Func.A(a,b) -> (fun p -> eval (b,(f p)))
    let eval (_,(t,_),f) = eval (t,f)*)

    let rec params : type a b. (a,b) Func.t -> param list -> param list = fun t l ->
      match t with
      | Func.R _ -> List.rev l
      | Func.A(a,b) -> params b (a::l)
    let params (_,(t,_),_) = params t []

    (* create wires for input parameters and instantiate the action body.  return
     * both as part of the rule action *)
    let rec inst : 
      type a. (a,R.defn) Func.t * a -> R.inst -> 
              R.inst * R.defn = fun (t,f) l ->
      match t with
      | Func.R p -> List.rev l, f
      | Func.A(a,b) -> 
        let w = HardCaml.Signal.Comb.wire (snd a) in
        inst (b, (f w)) ((a,w)::l) 
    let inst ~i ~s (_,(t,_),f) = 
      let f = f ~i ~s in
      { f with action = inst (t,f.action) [] }

    (* collect together the arguments for the method call and return them.
     * these need to get connected to the wires created with [inst]. *)
    let rec call : type a. (a,R.inst) Func.t -> R.inst -> a = fun t l ->
      match t with
      | Func.R p -> List.rev l
      | Func.A(a,b) -> (fun p -> call b ((a,p)::l)) 

    let call ((_,(_,t),_) : ('a,'b,'c,'d) defn) = call t []

  end

end

