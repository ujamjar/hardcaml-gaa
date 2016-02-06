open HardCaml.Signal
open State

type 'a t = 
  {
    guard : Comb.t;
    action : 'a;
  }

type unrule = i:inp_sig -> s:state_sig -> (state_sig t)

type inst = state_sig t

