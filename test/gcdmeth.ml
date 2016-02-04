#use "test/frl.ml";;


(* GCD controlled through methods *)
module Gcd = struct
  open Signal.Comb
 
  let name = "gcd_methods"

  module I = interface end
  module O = interface end
  module S = interface x[8] y[8] end
  let s' = S.(map (fun _ -> empty) t)

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

  module Meth = Meth(I)(S)
  module Start' = Meth.Make(Start)
  module Ready' = Meth.Make(Ready)

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


