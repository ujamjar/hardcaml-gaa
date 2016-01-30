#use "test/frl.ml";;

(* hand craft a few rules to test CF/SC *)
module TestSched = struct

  let name = "partsched"

  module I = interface end
  module S = interface
    a[1] b[1] c[1] d[1]
  end
  module O = S

  let na = S.(map (fun _ -> empty) t)
  let r_spec = { Signal.Seq.r_sync with Signal.Types.reg_enable = enable }
  let clear = S.(map (fun (_,b) -> zero b) t)
  let output _ s = s
end

module TestSched0 = struct
  include TestSched
  (* trivially CF and SC - rules do not access any shared state *)
  let rules _ = 
    let open S in
    [
      "a", (fun s -> {
        guard = s.a ==: s.b;
        action = { na with a = s.b; b = s.a };
      });
      "b", (fun s -> {
        guard = s.c ==: s.d;
        action = { na with c = s.d; d = s.c };
      });
    ]

end

module TestSched1 = struct
  include TestSched
  (* CF neither rules reads the state the other writes *)
  let rules _ = 
    let open S in
    [
      "a", (fun s -> {
        guard = s.a |: s.d;
        action = { na with a = s.c }
      });
      "b", (fun s -> {
        guard = s.b |: s.d;
        action = { na with b = s.c }
      });
    ]

end
module TestSched2 = struct
  include TestSched
  (* Not CF - both rules write state a, though neither reads it.  SC both ways *)
  let rules _ = 
    let open S in
    [
      "a", (fun s -> {
        guard = s.d;
        action = { na with a = s.c }
      });
      "b", (fun s -> {
        guard = s.b;
        action = { na with a = s.c }
      });
    ]
end
module TestSched3 = struct
  include TestSched
  (* SC a->b *)
  let rules _ = 
    let open S in
    [
      "a", (fun s -> { (* d=b,c,d r=a *)
        guard = s.d |: s.b;
        action = { na with a = s.c }
      });
      "b", (fun s -> { (* d=b,c,d r=a,b *)
        guard = s.b;
        action = { na with a = s.c; b = s.d }
      });
    ]
end
module TestSched4 = struct
  include TestSched
  (* SC b->a *)
  let rules i = 
    let open S in
    [
      "a", (fun s -> {
        guard = s.b;
        action = { na with a = s.c; b = s.d }
      });
      "b", (fun s -> { 
        guard = s.d |: s.b;
        action = { na with a = s.c }
      });
    ]
end

module X0 = Make(TestSched0)
module X1 = Make(TestSched1)
module X2 = Make(TestSched2)
module X3 = Make(TestSched3)
module X4 = Make(TestSched4)

