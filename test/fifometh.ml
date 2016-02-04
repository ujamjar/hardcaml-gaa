(* Rosenband, Fig 42, Pg 60. *)
#use "test/frl.ml";;

module FIFO = struct
  open Signal.Comb

  let name = "FIFO"

  module I = interface end
  module S = interface
    data0[8] data1[8]
    full0[1] full1[1]
  end
  module O = interface end

  let def = S.map (fun _ -> empty) S.t

  module Enq = struct
    open S open I
    module Args = interface x[8] end
    module Rets = interface end
    let meth = "enq", (fun i s a -> {
      guard = ~: (s.full1);
      action = {
        data1 = a.Args.x;
        full1 = s.full0;
        data0 = mux2 s.full0 s.data0 a.Args.x;
        full0 = vdd;
      }
    }, Rets.None)
  end

  module Deq = struct
    open S
    module Args = interface end
    module Rets = interface end
    let meth = "deq", (fun i s a -> {
      guard = s.full0;
      action = { def with
        full1 = gnd;
        full0 = s.full1;
        data0 = s.data1;
      };
    }, Rets.None)
  end

  module Clearf = struct
    module Args = interface end
    module Rets = interface end
    let meth = "clearf", (fun i s a -> {
      guard = vdd;
      action = { def with full1=gnd; full0=gnd };
    }, Rets.None)
  end

  module First = struct
    module Args = interface end
    module Rets = interface data[8] end
    let meth = "first", (fun i s a -> {
      guard = s.S.full0;
      action = def;
    }, Rets.{ data = s.data0 })
  end

  module Meth = Meth(I)(S)
  module Clearf' = Meth.Make(Clearf)
  module First' = Meth.Make(First)
  module Enq' = Meth.Make(Enq)
  module Deq' = Meth.Make(Deq)

  let methods = [
    Clearf'.meth;
    First'.meth;
    Enq'.meth;
    Deq'.meth;
  ]

  let rules = [ ]

  let r_spec = { Signal.Seq.r_sync with Signal.Types.reg_enable = enable }
  let clear i = S.(map (fun (_,b) -> zero b) t)
  let output i s = O.None

  let sched_opt = [ `cf; `me; `sc ]
  let me_rules = []

end

module X = Make(FIFO)
module S = Cyclesim.Api

let () = X.sim (fun ~sim ~clear ~enable ~i ~o ->
  let open X.G.I in
  S.reset sim;
  enable := B.vdd;
  clear := B.vdd;
  S.cycle sim;
  clear := B.gnd;
  
  (List.nth i.method_en 2) := B.vdd;
  (List.nth i.method_in 0) := B.consti 8 3;
  S.cycle sim;

  (List.nth i.method_en 2) := B.vdd;
  (List.nth i.method_in 0) := B.consti 8 4;
  S.cycle sim;

  (List.nth i.method_en 2) := B.gnd;
  (List.nth i.method_en 3) := B.vdd;
  S.cycle sim;
  S.cycle sim;
  (List.nth i.method_en 3) := B.gnd;
  S.cycle sim;
  S.cycle sim;
)


