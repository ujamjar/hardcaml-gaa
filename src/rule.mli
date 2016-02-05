open HardCaml.Signal

type 'a t = 
  {
    guard : Comb.t;
    action : 'a;
  }

type 'a state = (string * 'a) list

(*module St : State.S*)

type state_spec = int state
type inp_spec = int state
type out_spec = int state
type arg_spec = int state
type ret_spec = int state

type state_sig = Comb.t state
type inp_sig = Comb.t state
type out_sig = Comb.t state
type arg_sig = Comb.t state
type ret_sig = Comb.t state

type unrule = i:inp_sig -> s:state_sig -> (state_sig t)

type inst = state_sig t

type 'a methio_params = (string * Comb.t) list

type meth = {
  en : Comb.t;
  args : arg_sig;
  ret_spec : (string * int) list;
  fn : i:inp_sig -> s:state_sig -> a:arg_sig -> (state_sig t * ret_sig);
}

