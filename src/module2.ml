open HardCaml

type 'a rule = {
  guard : Signal.Comb.t;
  action : 'a;
}

(* creating unique identifiers *)
let mk_id() = 
  let id = ref (-1) in
  (fun () -> incr id; !id)

let inst_id = mk_id ()

(* global name mangler *)
let mangler = Circuit.Mangler.make []
let mangle s = Circuit.Mangler.mangle mangler s

module API
  (I : Interface.S)
  (S : Interface.S) 
  (O : Interface.S) = struct

  module Default_spec = struct
    open Signal
    let r_spec = { Seq.r_sync with Types.reg_enable = Comb.enable }
    let clear _ = S.(map (fun (_,b) -> Comb.zero b) t)
    let output _ _ = O.(map (fun (_,b) -> Comb.zero b) t)
    let sched_opt  = [ `cf; `me; `sc ]
    let me_rules = []
  end

  (* unique identifier for this instance *)
  let inst_id = inst_id()

  let rule ?(name="") fn = name, fn

  let rmethod ?(name="") typ meth = name, typ, meth

  let amethod ?(name="") typ meth = name, typ, meth

end

module type Modl = sig
  open Signal.Comb
  module I : Interface.S
  module S : Interface.S
  module O : Interface.S
  val name : string
  val r_spec : Signal.Types.register
  val clear : t I.t -> t S.t
  val output : t I.t -> t S.t -> t O.t
  val sched_opt : Sched.sched_opt list
  val me_rules : string list list
  val rules : (string * (i:t I.t -> s:t S.t -> t S.t rule)) list
  val methods : unit list (* XXX *)
end

module Top(M : Modl) = struct

  module T = Typ.Make(M.I)(M.S)(M.O)
  module Compile = Compile.Make(T)  (* HACKED *)

  let hack_rule (n,r) = (* HACKED *)
    n, (fun ~i ~s ->
      let r = r ~i ~s in
      { T.guard = r.guard; action = r.action })

  let f i = 
    let st_clear = M.clear i in
    let (en,s),metho = 
      Compile.Build.compile 
        ~sched_opt:M.sched_opt
        ~me_rules:M.me_rules
        ~r_spec:M.r_spec 
        ~st_clear
        ~methods:[]
        ~rules:(List.map hack_rule M.rules) (* HACKED *)
        ~i:i
    in
    M.output i s

  let circuit () = 
    let open Signal.Comb in
    let i = M.I.(map (fun (n,b) -> input n b) t) in
    let o = f i in
    let o = M.O.(to_list @@ map2 (fun (n,b) o -> output n o) t o) in
    Circuit.make M.name o

  module B = HardCaml.Bits.Comb.IntbitsList 
  module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
  module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
  module Waveterm_ui = HardCamlWaveLTerm.Ui.Make(B)(Waveterm_waves)

  let wave_cfg = 
    let open Waveterm_waves in
    let f = function (n,b) -> if b=1 then n, B else n, H in
    Some(
      [ f ("clock",1); f ("clear", 1); f ("enable",1); ] @
      M.I.(to_list @@ map f t) @ 
      M.O.(to_list @@ map f t) @ 
      M.S.(to_list @@ map f t)  
    )

  let sim tb = 
    let open HardCaml in
    let module Y = Interface.Gen(B)(M.I)(M.O) in
    let module S = Cyclesim.Api in
    
    let circ, sim, i, o, o' = Y.make M.name f in
    let clear = try S.in_port sim "clear" with _ -> ref B.empty in
    let enable = try S.in_port sim "enable" with _ -> ref B.empty in
    let sim, waves = Waveterm_sim.wrap ?cfg:wave_cfg sim in

    let () = tb ~sim ~clear ~enable ~i ~o ~o' in
    Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

  let autosim n = sim (fun ~sim ~clear ~enable ~i ~o ~o' ->
    let module S = HardCaml.Cyclesim.Api in
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


