open HardCaml.Signal.Comb

exception Error of (string * int) list * string list

module I2S(I : HardCaml.Interface.S)(S : State.S) : sig
  val to_intf_map : ('a -> 'b) -> 'a S.t -> 'b I.t
  val to_state_name_map : ('a -> 'b) -> (string * 'a) I.t -> 'b S.t
  val to_state_map : ('a -> 'b) -> 'a I.t -> 'b S.t
  val to_intf : 'a S.t -> 'a I.t
  val to_state_name : (string * 'a) I.t -> 'a S.t
  val to_state : 'a I.t -> 'a S.t
end 

module type S = sig
  module I : HardCaml.Interface.S
  module O : HardCaml.Interface.S
  module S : HardCaml.Interface.S
  val name : string
  val methods : (string * Rule.unmeth) list
  val rules : (string * (t I.t -> t S.t -> t S.t Rule.t)) list
  val r_spec : HardCaml.Signal.Types.register
  val clear : t I.t -> t S.t
  val output : t I.t -> t S.t -> t O.t
  val sched_opt : Sched.sched_opt list
  val me_rules : string list list
end

module Make(G : S) : sig

  val n_methods : int

  module I : interface
    (i : G.I)
    method_en{ }
    method_in{ }
  end
  module O : interface
    (o : G.O)
    method_vld{ }
    method_out{ }
    rules_running
  end

  val f : t I.t -> t O.t

  val circuit : string -> HardCaml.Circuit.t

end

