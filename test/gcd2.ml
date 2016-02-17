(* revised API *)

module I = interface x_in[8] y_in[8] end
module O = interface result[8] end
module S = interface x[8] y[8] end

let zero' (_,b) = HardCaml.Signal.Comb.zero b
let i' = I.(map zero' t)
let s' = S.(map zero' t)

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

let ready = 
  Gaa.T.Rmethod2.(define "ready" (returning !("result",8))) 
    (fun ~i ~s -> 
      {
        Gaa.T.guard = vdd;
        action = s.S.x;
      })

let start = 
  Gaa.T.Amethod.(define "start" (("x",8) @-> ("y",8) @-> returning ())) 
    (fun ~i ~s -> {
      Gaa.T.guard = vdd;
      action = (fun x y -> { S.x=x; y=y });
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

open HardCaml.Signal.Comb
open Gaa.T

let a, b, c = ("a",1), ("b",1),("c",1)



