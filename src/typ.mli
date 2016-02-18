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
           (O : HardCaml.Interface.S) : 
  S with type 'a I.t = 'a I.t
     and type 'a S.t = 'a S.t
     and type 'a O.t = 'a O.t
           

