open HardCaml

(* re-export *)
type 'a rule = 'a Typ.rule = {
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

module Method_inst = struct
  open HardCaml.Signal.Types
  type params = (string * int) list
  type ('a,'b) t = 
    | R of string * params * params * 'a
    | A of string * params * 'b
end

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

  module T = Typ.Make(I)(S)(O)

  let rmethod ?(name="") typ meth = 
    let (name,typ,fn) as defn = T.Rmethod2.define name typ meth in
    object
        method defn = defn
        method inst = 
          let p,r = T.Rmethod2.params defn in
          Method_inst.R(name, p, r, (fun ~i ~s -> T.Rmethod2.inst ~i ~s defn))
    end

  let amethod ?(name="") typ meth = 
    let (name,typ,fn) as defn = T.Amethod.define name typ meth in
    object
        method defn = defn
        method inst = 
          let p = T.Amethod.params defn in
          Method_inst.A(name, p, (fun ~i ~s -> T.Amethod.inst ~i ~s defn))
    end

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
  open HardCaml.Signal.Types
  val methods :
    ((i:signal I.t -> s:signal S.t -> 
        ((((string*int)*signal) list) * ((string*int)*signal) list) rule),
     (i:signal I.t -> s:signal S.t -> 
        ((((string*int)*signal) list) * signal S.t) rule))
    Method_inst.t list 
end

module Top(M : Modl) = struct

  module T = Typ.Make(M.I)(M.S)(M.O)
  module Compile = Compile.Make(T)  (* HACKED *)

  (* T.rule <> Module2.rule...FIXME *)
  (*let hack_rule (n,r) = (* HACKED *)
    n, (fun ~i ~s ->
      let r = r ~i ~s in
      { Typ.guard = r.guard; action = r.action })*)

  (* ... current implementation ... 
  type uninst_meth = {
    arg_spec : State.arg_spec;
    ret_spec : State.ret_spec;
    fn : i:signal I.t -> s:signal S.t -> a:State.arg_sig -> (inst_rule * State.ret_sig);
  }

  type inst_meth = {
    en : signal;
    args : State.arg_sig;
    spec : uninst_meth;
  }
  *)

  let show_method = 
    let open Printf in
    function
      | Method_inst.R(n,p,r,_) -> begin
        printf "rmethod %s: " n;
        List.iter (fun (n,b) -> printf "%s[%i] -> " n b) p;
        printf "(%s)\n" 
          (String.concat " * " 
            (List.map (fun (n,b) -> sprintf "%s[%i]" n b) r))
      end
      | Method_inst.A(n,p,_) -> begin
        printf "amethod %s: " n;
        List.iter (fun (n,b) -> printf "%s[%i] -> " n b) p;
        printf "()\n"
      end

  let methods m = (* XXX refactor types *)
    let open HardCaml.Signal.Comb in
    List.map (function
      | Method_inst.R(n,p,r,f) -> 
        let n = "rmethod_" ^ n in
        let en = input (n ^ "_EN") 1 in
        let ps = List.map (fun (n',b) -> n',input (n ^ "_arg_" ^ n') b) p in
        n, {
          T.en = en;
          args = State.Arg.of_list ps;
          spec = 
            {
              T.arg_spec = State.Arg.of_list p;
              ret_spec = State.Ret.of_list r;
              fn = 
                (fun ~i ~s ~a ->
                  (* instantiate the method *)
                  let r = f ~i ~s in
                  (* attach inputs *)
                  List.iter2 (fun (_,i) (_,j) -> i <== j) (fst r.action) ps;
                  { r with action=M.S.(map (fun _ -> empty) t) },
                  State.Ret.of_list (List.map (fun ((n,_),s) -> n,s) (snd r.action)));
            };
        }
      | Method_inst.A(n,p,f) ->
        let n = "amethod_" ^ n in
        let en = input (n ^ "_EN") 1 in
        let ps = List.map (fun (n',b) -> n',input (n ^ "_" ^ n' ^ "_ARG") b) p in
        n, {
          T.en = en;
          args = State.Arg.of_list ps;
          spec = 
            {
              T.arg_spec = State.Arg.of_list p;
              ret_spec = State.Ret.of_list [];
              fn = 
                (fun ~i ~s ~a -> 
                  (* instantiate the method *)
                  let r = f ~i ~s in
                  (* attach inputs *)
                  List.iter2 (fun (_,i) (_,j) -> i <== j) (fst r.action) ps;
                  { r with action=snd r.action; }, 
                  State.Ret.of_list []);
            };
        }) m

  let f i = 
    let st_clear = M.clear i in
    List.iter show_method M.methods;
    let (en,s),metho = 
      Compile.Build.compile 
        ~sched_opt:M.sched_opt
        ~me_rules:M.me_rules
        ~r_spec:M.r_spec 
        ~st_clear
        ~methods:(methods M.methods)
        ~rules:M.rules
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


