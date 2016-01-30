open HardCaml
open Signal.Comb
open HardCamlGaa
open Frl_hw

module B = Bits.Comb.IntbitsList 

module type Gaa = sig
  include Gaa
  val name : string
  val rules : t I.t -> (string * (t S.t -> t S.t rule)) list
end

module Make(G : Gaa) = struct

  module X = Gaa(G)

  module O_run = interface (o : G.O) gaa_running[1] end

  let vlog () = 
    let circ = X.circuit G.name G.rules in
    Rtl.Verilog.write print_string circ

  module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
  module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
  module Waveterm_ui = HardCamlWaveLTerm.Ui.Make(B)(Waveterm_waves)

  let wave_cfg = 
    let open Waveterm_waves in
    let f = function (n,b) -> if b=1 then n, B else n, H in
    Some(
      [ f ("clock",1); f ("clear", 1); f ("enable",1); f ("gaa_running",1) ] @
      G.I.(to_list @@ map f t) @ 
      G.O.(to_list @@ map f t) @ 
      G.S.(to_list @@ map f t)  
    )

  let sim f = 
    let module Y = Interface.Gen(B)(G.I)(O_run) in
    let module S = Cyclesim.Api in
    
    let hw_f i = (* decorate outputs with rule status *)
      let o, gaa_running = X.f_en G.rules i in
      O_run.{ o; gaa_running }
    in
    let circ, sim, i, o, _ = Y.make G.name hw_f in
    let clear = S.in_port sim "clear" in
    let enable = S.in_port sim "enable" in
    let running = S.out_port sim "gaa_running" in
    let sim, waves = Waveterm_sim.wrap ?cfg:wave_cfg sim in

    let () = f ~sim ~clear ~enable ~running ~i ~o in
    Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

  let autosim n = sim (fun ~sim ~clear ~enable ~running ~i ~o ->
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

