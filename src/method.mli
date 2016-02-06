open HardCaml.Signal
open State

type unmeth = {
  arg_spec : arg_spec;
  ret_spec : ret_spec;
  fn : i:inp_sig -> s:state_sig -> a:arg_sig -> (state_sig Rule.t * ret_sig);
}

type meth = {
  en : Comb.t;
  args : arg_sig;
  spec : unmeth;
}
