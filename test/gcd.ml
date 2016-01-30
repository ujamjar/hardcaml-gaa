#use "test/frl.ml";;

(* GAA MODULE *)
module Gcd = struct
  open Signal.Comb
 
  (* module name *)
  let name = "gcd"

  (* input and output interfaces *)
  module I = interface x_in[8] y_in[8] end
  module O = interface result[8] end

  (* atomic state *)
  module S = interface x[8] y[8] end

  (* atomic state update rules *)
  let rules i = 
    let open S in
    [
      "sub", (fun s -> {
        guard  = ((s.x >=: s.y) &: (s.y <>:. 0));
        action = { x = (s.x -: s.y); y = empty; };
      });
          
      "swap", (fun s -> {
        guard  = ((s.x <: s.y) &: (s.y <>:. 0));
        action = { x = s.y; y = s.x };
      });
    ]

  (* state register type - syncronous clear with global enable *)
  let r_spec = { Signal.Seq.r_sync with Signal.Types.reg_enable = enable }

  (* state register initial values *)
  let clear i = S.{
    x = i.I.x_in;
    y = i.I.y_in;
  }

  (* construct output *)
  let output i s = O.{ result = s.S.x }

end

(* TESTBENCH *)
module X = Make(Gcd)
module S = Cyclesim.Api

let () = X.sim (fun ~sim ~clear ~enable ~running ~i ~o ->
  S.reset sim;
  List.iter 
    (fun (x,y) ->
      clear := B.vdd;
      i.Gcd.I.x_in := B.consti 8 x;
      i.Gcd.I.y_in := B.consti 8 y;
      S.cycle sim;
      clear := B.gnd;
      enable := B.vdd;
      S.cycle sim;
      while !running = B.vdd do
        S.cycle sim
      done)
    [ (15,10); (9,3); (12,144) ]
)

