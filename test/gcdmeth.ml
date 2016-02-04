#use "test/frl.ml";;

module Of_list(X : HardCaml.Interface.S) : sig
  val of_list : 'a list -> 'a X.t
end = struct
  let of_list l = 
    let l = List.map2 (fun (n,b) s -> n,s) X.(to_list t) l in
    X.(map (fun (n,_) -> List.assoc n l) t)
end


(* GCD controlled through methods *)
module Gcd = struct
  open Signal.Comb
 
  let name = "gcd_methods"

  module I = interface end
  module O = interface end
  module S = interface x[8] y[8] end
  let s' = S.(map (fun _ -> empty) t)

  module type Meth = sig
    module Args : HardCaml.Interface.S
    module Rets : HardCaml.Interface.S
    val meth : string * (t I.t -> t S.t -> t Args.t -> t S.t rule * t Rets.t)
  end

  module Mk_meth(M : Meth) : sig
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

  (* action method *)
  module Start = struct
    module Args = interface x[8] y[8] end
    module Rets = interface end
    let meth = "start", (fun i s a -> {
        guard = s.S.y ==:. 0;
        action = 
          { S.x = a.Args.x; y = a.Args.y } (* action *)
      }, Rets.None)
  end

  (* read method *)
  module Ready = struct
    module Args = interface end
    module Rets = interface result[8] end
    let meth = "ready", (fun i s a ->
      {
        guard = s.S.y ==:. 0;
        action = s';
      }, Rets.{ result = s.S.y })
  end

  module Start' = Mk_meth(Start)
  module Ready' = Mk_meth(Ready)

  let methods = [ 
    Start'.meth; 
    Ready'.meth;
  ]

  (* atomic state update rules *)
  let rules = 
    let open S in
    [
      "sub", (fun i s -> {
        guard  = ((s.x >=: s.y) &: (s.y <>:. 0));
        action = { x = (s.x -: s.y); y = empty; };
      });
          
      "swap", (fun i s -> {
        guard  = ((s.x <: s.y) &: (s.y <>:. 0));
        action = { x = s.y; y = s.x };
      });
    ]

  (* state register type - syncronous clear with global enable *)
  let r_spec = { Signal.Seq.r_sync with Signal.Types.reg_enable = enable }

  (* state register initial values *)
  let clear i = S.(map (fun (n,b) -> zero b) t)

  (* construct output *)
  let output i s = O.None

  (* rule parallelism to find *)
  let sched_opt = [ `cf; `me; `sc ]

  (* mutually exclusive rules *)
  let me_rules = [ ["swap"; "sub" ] ]

end

(* TESTBENCH *)
module X = Make(Gcd)
module S = Cyclesim.Api

let () = X.sim (fun ~sim ~clear ~enable ~i ~o ->
  let open X.G.I in
  S.reset sim;
  enable := B.vdd;
  (List.nth i.method_en 0) := B.vdd;
  (List.nth i.method_in 0) := B.consti 8 32;
  (List.nth i.method_in 1) := B.consti 8 24;
  S.cycle sim;
  (List.nth i.method_en 0) := B.gnd;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim;
)


