(* revised API *)

module I = interface x_in[8] y_in[8] end
module O = interface result[8] end
module S = interface x[8] y[8] end

module Gaa = Module.Make(I)(S)(O)(struct
  include Module.Default_params(I)(S)(O)
  let name = "gcd"
  let clear i = S.{
    x = i.I.x_in;
    y = i.I.y_in;
  }
  let output i s = O.{ result = s.S.x }
  let me_rules = [ ["swap"; "sub" ] ]
end)

open HardCaml.Signal.Comb
open S

let () = 

  Gaa.rule "sub" Gaa.T.(fun ~i ~s -> {
    guard  = ((s.x >=: s.y) &: (s.y <>:. 0));
    action = { x = (s.x -: s.y); y = empty; };
  });

  Gaa.rule "swap" Gaa.T.(fun ~i ~s -> {
    guard  = ((s.x <: s.y) &: (s.y <>:. 0));
    action = { x = s.y; y = s.x };
  })


module X = Gaa.Top ()

let tb ~sim ~clear ~enable ~i ~o ~o' =
  let module S = HardCaml.Cyclesim.Api in
  let module B = X.B in
  let running = o.X.O.rules_running in
  let i = i.X.I.i in
  S.reset sim;
  List.iter 
    (fun (x,y) ->
      clear := B.vdd;
      i.I.x_in := B.consti 8 x;
      i.I.y_in := B.consti 8 y;
      S.cycle sim;
      clear := B.gnd;
      enable := B.vdd;
      S.cycle sim;
      while !running = B.vdd do
        S.cycle sim
      done)
    [ (15,10); (9,3); (12,144) ]
