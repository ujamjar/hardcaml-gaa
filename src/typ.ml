module type S = sig
  open HardCaml.Signal.Types

  module I : HardCaml.Interface.S 
  module S : HardCaml.Interface.S 
  module O : HardCaml.Interface.S 

  (* rules *)
  type 'a rule = 
    {
      guard : signal;
      action : 'a;
    }

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

end

module Make(I : HardCaml.Interface.S)
           (S : HardCaml.Interface.S) 
           (O : HardCaml.Interface.S) = struct

  open HardCaml.Signal.Types

  module I = I
  module S = S
  module O = O

  (* rules *)
  type 'a rule = 
    {
      guard : signal;
      action : 'a;
    }

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

end

