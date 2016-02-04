open HardCaml
open Signal.Comb
open HardCamlGaa
open Frl_hw

module B = Bits.Comb.IntbitsList 

module Of_list(X : HardCaml.Interface.S) : sig
  val of_list : 'a list -> 'a X.t
end = struct
  let of_list l = 
    let l = List.map2 (fun (n,b) s -> n,s) X.(to_list t) l in
    X.(map (fun (n,_) -> List.assoc n l) t)
end

module Meth(I : HardCaml.Interface.S)(S : HardCaml.Interface.S) = struct

  module type Meth = sig
    module Args : HardCaml.Interface.S
    module Rets : HardCaml.Interface.S
    val meth : string * (t I.t -> t S.t -> t Args.t -> t S.t rule * t Rets.t)
  end

  module Make(M : Meth) : sig
    val meth : 
      string * (string*int) list * (string*int) list * 
      (t I.t -> t S.t -> t list -> (t S.t rule * t list))
  end = struct
    module Al = Of_list(M.Args)
    let meth = 
      let name, ameth = M.meth in
      name, M.Args.(to_list t), M.Rets.(to_list t),
        (fun i s arg -> 
          let rule, rets = ameth i s (Al.of_list arg) in
          rule, M.Rets.to_list rets)
  end

end

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


