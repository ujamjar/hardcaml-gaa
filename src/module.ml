open HardCaml.Signal.Comb

exception Error of (string * int) list * string list

module I2S(I : HardCaml.Interface.S)(S : State.S) = struct

  let to_intf_map f s = 
    try 
      let s = S.to_list s in
      I.(map (fun (n,_) -> f (List.assoc n s)) t)
    with _ ->
      raise (Error(I.to_list I.t, List.map fst @@ S.to_list s))
      
  let to_state_name_map f i = 
    S.of_list @@ List.map (fun (n,x) -> n, f x) @@ I.(to_list i)

  let to_state_map f i = 
    S.of_list I.(to_list @@ map2 (fun (n,_) s -> n,f s) t i)

  let to_intf s = to_intf_map (fun x -> x) s

  let to_state_name s = to_state_name_map (fun x -> x) s

  let to_state s = to_state_map (fun x -> x) s

end

module type S = sig
  module I : HardCaml.Interface.S
  module O : HardCaml.Interface.S
  module S : HardCaml.Interface.S
  val name : string
  val methods : (string * Method.unmeth) list
  val rules : (string * (t I.t -> t S.t -> t S.t Rule.t)) list
  val r_spec : HardCaml.Signal.Types.register
  val clear : t I.t -> t S.t
  val output : t I.t -> t S.t -> t O.t
  val sched_opt : Sched.sched_opt list
  val me_rules : string list list
end

module Make(G : S) = struct

  module Il = I2S(G.I)(State.Inp)
  module Sl = I2S(G.S)(State.State)
  module Ol = I2S(G.O)(State.Out)

  let n_methods = List.length G.methods

  let n_method_inputs =
    List.fold_left (fun acc (_,m) -> acc + State.Arg.length m.Method.arg_spec) 0 G.methods 
  
  let n_method_outputs = 
    List.fold_left (fun acc (_,m) -> acc + State.Ret.length m.Method.ret_spec) 0 G.methods 

  let methods = 
    let port io n' (n,b) = "method_" ^ io ^ "_" ^ n' ^ "_" ^ n, b in
    List.map (fun (n,m) -> 
      n,
      { m with
        Method.arg_spec = State.Arg.map_t (port "in" n) m.Method.arg_spec; 
        ret_spec = State.Ret.map_t (port "out" n) m.Method.ret_spec }) 
    G.methods

  module I' = interface
    (i : G.I)
    method_en{n_methods}[1]
    method_in{n_method_inputs}
  end
  module O' = interface
    (o : G.O)
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
          List.map (fun (_,m) -> State.Arg.to_list m.Method.arg_spec) methods;
      }
  end

  module O = struct
    include O' 
    let t = 
      O'.{ t with
        method_vld = List.map (fun (n,_) -> "method_vld_" ^ n, 1) methods;
        method_out = List.concat @@ 
          List.map (fun (_,m) -> State.Ret.to_list m.Method.ret_spec) methods;
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
        let a, args = g (State.Arg.to_list m.Method.arg_spec) args [] in
        (n, { Method.en=e; args=State.Arg.of_list a; spec=m }) :: f e' m' args
      | _ -> failwith "imbalanced lists"
    in
    f i.I.method_en G.methods i.I.method_in

  let f i =
    let methods = build_methods i in
    let st_clear = Sl.to_state (G.clear i.I.i) in
    let i' = Il.to_state i.I.i in
    let s = Sl.to_state_name G.S.t in
    let rules = List.map 
      (fun (n,f) -> 
        let f ~i ~s = 
          let r = f (Il.to_intf i) (Sl.to_intf s) in
          Rule.{ r with action = Sl.to_state r.action }
        in
        n, f) G.rules
    in
    let (en,s),metho = 
      Compile.Build.compile 
        ~sched_opt:G.sched_opt
        ~me_rules:G.me_rules
        ~r_spec:G.r_spec ~st_clear
        ~methods ~rules ~i:i' ~s
    in
    let get_meth_out (_,x) = List.map snd @@ State.Ret.to_list x in
    O.{
      rules_running = en;
      method_vld = List.map fst metho;
      o = G.output i.I.i Sl.(to_intf s);
      method_out = List.concat @@ List.map get_meth_out metho;
    }

  let circuit name = 
    let i = I.(map (fun (n,b) -> input n b) t) in
    let o = f i in
    let o = O.(to_list @@ map2 (fun (n,b) o -> output n o) t o) in
    HardCaml.Circuit.make name o

end


