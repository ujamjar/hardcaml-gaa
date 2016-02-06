#use "test/frl.ml";;

(* Instead of using a global clear signal to initialise the system state,
 * intead we have a clear-like method.  This will initialise the registers
 * based on input signals, but will be a rule guarded by 'i.clear'.
 *
 * One thing to note; the clear method will be SC(21) to all other rules as
 * it reads no part of the state.  Thus it will still allow sceduling groups to
 * be formed.
 *)
module Clear = struct
  open Signal.Comb
 
  let name = "clear_test"

  module I = interface clear[1] x[8] y[8] end
  module O = interface ox[8] oy[8] end

  module S = interface x[8] y[8] end
  let s' = S.(map (fun _ -> empty) t)

  let methods = []

  let rules = 
    let open S in
    [
      "clear", (fun i s -> {
        guard  = i.I.clear;
        action = { x = i.I.x; y = i.I.y }
      });
          
      "runx", (fun i s -> {
        guard  = vdd;
        action = { s' with x = s.x +:. 1 };
      });

      "runy", (fun i s -> {
        guard  = vdd;
        action = { s' with y = s.y -:. 1 };
      });
    ]

  let r_spec = { Signal.Seq.r_none with Signal.Types.reg_enable = enable }

  let clear i = S.(map (fun (n,b) -> zero b) t)

  let output i s = O.{ ox = s.S.x; oy = s.S.y; }

  let sched_opt = [ `cf; `me; `sc ]

  let me_rules = [ ]

end

(* TESTBENCH *)
module X = Make(Clear)
module S = Cyclesim.Api

let () = X.sim (fun ~sim ~clear ~enable ~i ~o ->
  let i = i.X.G.I.i in
  S.reset sim;
  enable := B.vdd;
  S.cycle sim;
  S.cycle sim;
  i.Clear.I.clear := B.vdd;
  i.Clear.I.x := B.consti 8 32;
  i.Clear.I.y := B.consti 8 16;
  S.cycle sim;
  i.Clear.I.clear := B.gnd;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim;
)


