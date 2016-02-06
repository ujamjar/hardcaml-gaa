open HardCaml
open Signal.Comb
open HardCamlGaa

module B = Bits.Comb.IntbitsList 

module Meth(I : HardCaml.Interface.S)(S : HardCaml.Interface.S) = struct

  module type Meth = sig
    module Args : HardCaml.Interface.S
    module Rets : HardCaml.Interface.S
    val meth : string * (t I.t -> t S.t -> t Args.t -> t S.t Rule.t * t Rets.t)
  end

  module Il = Module.I2S(I)(State.Inp)
  module Sl = Module.I2S(S)(State.State)

  module Make(M : Meth) : sig
    val meth : string * Rule.unmeth
  end = struct
    module Al = Module.I2S(M.Args)(State.Arg)
    module Rl = Module.I2S(M.Rets)(State.Ret)
    let meth = 
      let name, meth = M.meth in
      let arg_spec = Al.to_state_map snd M.Args.t in
      let ret_spec = Rl.to_state_map snd M.Rets.t in
      let fn ~i ~s ~a = 
        let ru, ret = meth (Il.to_intf i) (Sl.to_intf s) (Al.to_intf a) in
        Rule.{ ru with action = Sl.to_state ru.action },
        Rl.to_state ret
      in
      name, Rule.{ arg_spec; ret_spec; fn }

  end

end

module Make(G' : Module.S) = struct

  module G = Module.Make(G')

  let vlog f = 
    let circ = G.circuit G'.name in
    Rtl.Verilog.write (output_string f) circ

  module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
  module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
  module Waveterm_ui = HardCamlWaveLTerm.Ui.Make(B)(Waveterm_waves)

  let wave_cfg = 
    let open Waveterm_waves in
    let f = function (n,b) -> if b=1 then n, B else n, H in
    Some(
      [ f ("clock",1); f ("clear", 1); f ("enable",1); ] @
      G.I.(to_list @@ map f t) @ 
      G.O.(to_list @@ map f t) @ 
      G'.S.(to_list @@ map f t)  
    )

  let sim f = 
    let module Y = Interface.Gen(B)(G.I)(G.O) in
    let module S = Cyclesim.Api in
    
    let circ, sim, i, o, _ = Y.make G'.name G.f in
    let clear = try S.in_port sim "clear" with _ -> ref B.empty in
    let enable = try S.in_port sim "enable" with _ -> ref B.empty in
    let sim, waves = Waveterm_sim.wrap ?cfg:wave_cfg sim in

    let () = f ~sim ~clear ~enable ~i ~o in
    Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

  let autosim n = sim (fun ~sim ~clear ~enable ~i ~o ->
    let module S = Cyclesim.Api in
    S.reset sim;
    clear := B.vdd;
    S.cycle sim;
    clear := B.gnd;
    enable := B.vdd;
    for i=0 to n-1 do
      S.cycle sim
    done;
    S.cycle sim
  )

end

let with_out_file fname g = 
  let f = open_out fname in
  let r = g f in
  close_out f;
  r

open Rule
