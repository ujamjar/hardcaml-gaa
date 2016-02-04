open HardCaml
open Signal.Comb
open HardCamlGaa
open Frl_hw

module B = Bits.Comb.IntbitsList 

module Make(G' : Gaa) = struct

  module G = Gaa(G')

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


