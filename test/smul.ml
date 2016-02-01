#use "test/frl.ml";;

module Smul = struct

  let name = "serial_mult"

  module I = interface x[16] y[16] end
  module O = interface product[32] end
  module S = interface product[32] d[32] r[16] end

  let rules i = 
    let open S in
    [
      "cycle", fun s -> {
        guard = s.r <>:. 0;
        action = { 
          product = mux2 (lsb s.r) (s.product +: s.d) s.product;
          d = sll s.d 1;
          r = srl s.r 1;
        }
      }
    ]

  let r_spec = { Signal.Seq.r_sync with Signal.Types.reg_enable = enable }

  let clear i = S.{
    product = zero 32;
    d = sresize i.I.x 32;
    r = i.I.y;
  }

  let output i s = O.{ product = s.S.product }

  let sched_opt = [ `cf; `me; `sc ]
  let me_rules = [ ]
end

module X = Make(Smul)
module S = Cyclesim.Api

let () = X.sim (fun ~sim ~clear ~enable ~running ~i ~o ->
  S.reset sim;
  List.iter 
    (fun (x,y) ->
      enable := B.vdd;
      clear := B.vdd;
      i.Smul.I.x := B.consti 16 x;
      i.Smul.I.y := B.consti 16 y;
      S.cycle sim;
      clear := B.gnd;
      S.cycle sim;
      while !running = B.vdd do
        S.cycle sim
      done)
    [ (15,10); (9,3); (12,144) ]
)

