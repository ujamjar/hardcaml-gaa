(* Rosenband, Fig 42, Pg 60. *)
#use "test/frl.ml";;

module FIFO = struct
  open Signal.Comb

  let name = "FIFO"

  module I = interface
    enq[1] deq[1] clearf[1] first[1]
    enq_x[8]
  end
  module S = interface
    data0[8] data1[8]
    full0[1] full1[1]
  end
  module O = interface
    data[8]
  end

  let def = S.map (fun _ -> empty) S.t

  let methods = []

  let rules = 
    let open I in
    let open S in
    [
      "enq", (fun i s -> {
        guard = i.enq &: (~: (s.full1));
        action = {
          data1 = i.enq_x;
          full1 = s.full0;
          data0 = mux2 s.full0 s.data0 i.enq_x;
          full0 = vdd;
        };
      });

      "deq", (fun i s -> {
        guard = i.deq &: s.full0;
        action = { def with
          full1 = gnd;
          full0 = s.full1;
          data0 = s.data1;
        };
      });

      "clearf", (fun i s -> {
        guard = i.clearf;
        action = { def with full1=gnd; full0=gnd };
      });

      "first", (fun i s -> {
        guard = i.first &: s.full0;
        action = def;
      });
    ]

  let r_spec = { Signal.Seq.r_sync with Signal.Types.reg_enable = enable }
  let clear i = S.(map (fun (_,b) -> zero b) t)
  let output i s = O.{ data = s.S.data0 }

  let sched_opt = [ `cf; `me; `sc ]
  let me_rules = []

end

module X = Make(FIFO)
module S = Cyclesim.Api

let () = X.sim (fun ~sim ~clear ~enable ~i ~o ->
  let open FIFO.I in
  let i = i.X.G.I.i in
  let open FIFO.I in
  S.reset sim;
  enable := B.vdd;
  clear := B.vdd;
  S.cycle sim;
  clear := B.gnd;
  
  i.enq := B.vdd;
  i.enq_x := B.consti 8 3;
  S.cycle sim;

  i.enq := B.vdd;
  i.enq_x := B.consti 8 4;
  S.cycle sim;

  i.enq := B.gnd;
  i.deq := B.vdd;
  S.cycle sim;
  S.cycle sim;
  i.deq := B.gnd;
  S.cycle sim;
  S.cycle sim;
)

