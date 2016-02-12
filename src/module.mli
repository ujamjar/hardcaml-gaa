open HardCaml.Signal.Comb

exception Error of (string * int) list * string list

module Default_params
  (I : HardCaml.Interface.S)
  (S : HardCaml.Interface.S) 
  (O : HardCaml.Interface.S) : sig
  val r_spec : HardCaml.Signal.Types.register
  val clear : t I.t -> t S.t
  val output : t I.t -> t S.t -> t O.t
  val sched_opt : Sched.sched_opt list
  val me_rules : string list list
end

module Make(I : HardCaml.Interface.S)
           (S : HardCaml.Interface.S) 
           (O : HardCaml.Interface.S) 
           (P : sig
             val name : string
             val r_spec : HardCaml.Signal.Types.register
             val clear : t I.t -> t S.t
             val output : t I.t -> t S.t -> t O.t
             val sched_opt : Sched.sched_opt list
             val me_rules : string list list
           end)
           : sig

  module T : Typ.S
    with type 'a I.t = 'a I.t
     and type 'a S.t = 'a S.t
     and type 'a O.t = 'a O.t

  val rule : string -> T.uninst_rule -> unit
  val meth : string -> T.uninst_meth -> unit

  module Top( ) : sig

    open HardCaml

    val n_methods : int

    module I : interface
      (i : I)
      method_en{ }
      method_in{ }
    end
    module O : interface
      (o : O)
      method_vld{ }
      method_out{ }
      rules_running
    end

    val f : t I.t -> t O.t

    val circuit : unit -> Circuit.t

    module B : Comb.S

    val sim : (sim:B.t Cyclesim.Api.cyclesim -> clear:B.t ref -> enable:B.t ref -> 
               i:B.t ref I.t -> o:B.t ref O.t -> o':B.t ref O.t -> unit) -> unit

    val autosim : int -> unit
            
  end

end



