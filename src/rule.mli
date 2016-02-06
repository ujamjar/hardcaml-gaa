open HardCaml.Signal

type 'a t = 
  {
    guard : Comb.t;
    action : 'a;
  }

type state_spec = int State.State.t
type inp_spec = int State.Inp.t
type out_spec = int State.Out.t
type arg_spec = int State.Arg.t
type ret_spec = int State.Ret.t

type state_sig = Comb.t State.State.t
type inp_sig = Comb.t State.Inp.t
type out_sig = Comb.t State.Out.t
type arg_sig = Comb.t State.Arg.t
type ret_sig = Comb.t State.Ret.t

type unrule = i:inp_sig -> s:state_sig -> (state_sig t)

type inst = state_sig t

type unmeth = {
  arg_spec : arg_spec;
  ret_spec : ret_spec;
  fn : i:inp_sig -> s:state_sig -> a:arg_sig -> (state_sig t * ret_sig);
}

type meth = {
  en : Comb.t;
  args : arg_sig;
  spec : unmeth;
}
