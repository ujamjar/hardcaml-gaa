open HardCaml.Signal.Comb

exception Error of (string * int) list * string list

module Default_params
  (I : HardCaml.Interface.S)
  (S : HardCaml.Interface.S) 
  (O : HardCaml.Interface.S) = struct
  let r_spec = { HardCaml.Signal.Seq.r_sync with HardCaml.Signal.Types.reg_enable = enable }
  let clear _ = S.(map (fun (_,b) -> zero b) t)
  let output _ _ = O.(map (fun (_,b) -> zero b) t)
  let sched_opt  = [ `cf; `me; `sc ]
  let me_rules = []
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
           = struct

  module T = Typ.Make(I)(S)(O)
  module Compile = Compile.Make(T)

  let rules = ref []
  let meths = ref []

  let rule name rule = rules := (name,rule) :: !rules
  let meth name meth = meths := (name,meth) :: !meths

  module Top( ) = struct

    let rules = List.rev !rules
    let meths = List.rev !meths

    let n_methods = List.length meths

    let n_method_inputs =
      List.fold_left (fun acc (_,m) -> acc + State.Arg.length m.T.arg_spec) 0 meths
    
    let n_method_outputs = 
      List.fold_left (fun acc (_,m) -> acc + State.Ret.length m.T.ret_spec) 0 meths 

    let methods = 
      let port io n' (n,b) = "method_" ^ io ^ "_" ^ n' ^ "_" ^ n, b in
      List.map (fun (n,m) -> 
        n,
        { m with
          T.arg_spec = State.Arg.map_t (port "in" n) m.T.arg_spec; 
          ret_spec = State.Ret.map_t (port "out" n) m.T.ret_spec }) 
      meths

    module I' = interface
      (i : I)
      method_en{n_methods}[1]
      method_in{n_method_inputs}
    end
    module O' = interface
      (o : O)
      method_vld{n_methods}[1]
      method_out{n_method_outputs}
      rules_running[1]
    end

    module I = struct
      include I' 
      let t = 
        I'.{ t with
          method_en = List.map (fun (n,_) -> "method_en_" ^ n, 1) methods;
          method_in = List.concat @@ 
            List.map (fun (_,m) -> State.Arg.to_list m.T.arg_spec) methods;
        }
    end

    module O = struct
      include O' 
      let t = 
        O'.{ t with
          method_vld = List.map (fun (n,_) -> "method_vld_" ^ n, 1) methods;
          method_out = List.concat @@ 
            List.map (fun (_,m) -> State.Ret.to_list m.T.ret_spec) methods;
        }
    end

    let build_methods i =
      let rec g a b c = 
        match a, b with
        | [], _ -> List.rev c, b
        | (n,_)::a', b::b' -> g a' b' ((n,b)::c)
        | _, [] -> failwith "imbalanced in ports"
      in
      let rec f e m args = 
        match e,m with
        | [], [] -> []
        | e::e', (n,m)::m' ->
          let a, args = g (State.Arg.to_list m.T.arg_spec) args [] in
          (n, { T.en=e; args=State.Arg.of_list a; spec=m }) :: f e' m' args
        | _ -> failwith "imbalanced lists"
      in
      f i.I.method_en meths i.I.method_in

    let f i =
      let methods = build_methods i in
      let st_clear = P.clear i.I.i in
      let (en,s),metho = 
        Compile.Build.compile 
          ~sched_opt:P.sched_opt
          ~me_rules:P.me_rules
          ~r_spec:P.r_spec 
          ~st_clear
          ~methods 
          ~rules 
          ~i:i.I.i
      in
      let get_meth_out (_,x) = List.map snd @@ State.Ret.to_list x in
      O.{
        rules_running = en;
        method_vld = List.map fst metho;
        o = P.output i.I.i s;
        method_out = List.concat @@ List.map get_meth_out metho;
      }

    let circuit () = 
      let i = I.(map (fun (n,b) -> input n b) t) in
      let o = f i in
      let o = O.(to_list @@ map2 (fun (n,b) o -> output n o) t o) in
      HardCaml.Circuit.make P.name o
  
    
    module B = HardCaml.Bits.Comb.IntbitsList 
    module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
    module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
    module Waveterm_ui = HardCamlWaveLTerm.Ui.Make(B)(Waveterm_waves)

    let wave_cfg = 
      let open Waveterm_waves in
      let f = function (n,b) -> if b=1 then n, B else n, H in
      Some(
        [ f ("clock",1); f ("clear", 1); f ("enable",1); ] @
        I.(to_list @@ map f t) @ 
        O.(to_list @@ map f t) @ 
        S.(to_list @@ map f t)  
      )

    let sim tb = 
      let open HardCaml in
      let module Y = Interface.Gen(B)(I)(O) in
      let module S = Cyclesim.Api in
      
      let circ, sim, i, o, o' = Y.make P.name f in
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

end



