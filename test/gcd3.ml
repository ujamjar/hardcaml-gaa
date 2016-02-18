

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
    Module2.guard = ((s.x >=: s.y) &: (s.y <>:. 0));
    action = { x = (s.x -: s.y); y = empty; };
  })

  let swap = API.rule ~name:"swap" (fun ~i ~s -> {
    Module2.guard = ((s.x <: s.y) &: (s.y <>:. 0));
    action = { x = s.y; y = s.x };
  })

  let ready_t = API.T.Rmethod2.(returning !("result",8))
  let ready = API.rmethod ~name:"ready" ready_t (fun ~i ~s -> {
    Module2.guard = vdd;
    action = s.S.x;
  })

  let start_t = API.T.Amethod.(("x",8) @-> ("y",8) @-> returning ()) 
  let start = API.amethod ~name:"start" start_t (fun ~i ~s -> {
    Module2.guard = vdd;
    action = (fun x y -> { S.x=x; y=y });
  })

  let start100_t = API.T.Amethod.(("x",8) @-> returning ()) 
  let start100 = API.amethod ~name:"start100" start100_t (fun ~i ~s -> {
    Module2.guard = vdd;
    action = (fun x -> { S.x=x; y=consti 8 100 });
  })

  let rules = [ sub; swap ]
  let methods = [
    ready#inst;
    start#inst;
    start100#inst;
  ]

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

