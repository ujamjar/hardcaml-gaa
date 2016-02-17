

module Gcd = struct
  open HardCaml.Signal.Comb

  module I = interface x_in[8] y_in[8] end
  module S = interface x[8] y[8] end
  module O = interface result[8] end

  module API = Module2.API(I)(S)(O)
  include API.Default_spec 

  open S

  let name = "gcd"

  let sub = API.rule ~name:"sub" (fun ~i ~s -> {
    Module2.guard  = ((s.x >=: s.y) &: (s.y <>:. 0));
    action = { x = (s.x -: s.y); y = empty; };
  })

  let swap = API.rule ~name:"swap" (fun ~i ~s -> {
    Module2.guard  = ((s.x <: s.y) &: (s.y <>:. 0));
    action = { x = s.y; y = s.x };
  })

  let rules = [ sub; swap ]
  let methods = []

  let clear i = { S.x = i.I.x_in; y = i.I.y_in }
  let output i s = O.{ result = s.S.x }

end

module Gcd_top = Module2.Top(Gcd)

let tb ~sim ~clear ~enable ~i ~o ~o' =
  let module S = HardCaml.Cyclesim.Api in
  let module B = HardCaml.Bits.Comb.IntbitsList in
  let running = ref B.gnd (*o.Gcd.O.rules_running*) in
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

