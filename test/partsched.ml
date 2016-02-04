#use "test/frl.ml";;

(* example given in thesis *)
module PartSched = struct

  let name = "partsched"

  module I = interface end
  module S = interface
    t{|6|}[1]
  end
  module O = S

  let cfp = [ (* pairs of conflict free rules *)
    1,2; 1,3; 1,5; 1,6;
    2,3; 2,4; 2,6;
    3,4; 3,5; 3,6;
    4,5;
    5,6
  ]
  let is_cf i j = 
    if i<j then List.mem (i,j) cfp
    else List.mem (j,i) cfp

  let methods = []

  let rules = 
    let open S in
    let cf i = 
      fun _(s : HardCaml.Signal.Comb.t S.t) -> { 
        guard = vdd;
        action = {
          t = Array.init 6 (fun j -> 
          if i=(j+1) then 
            reduce (|:) 
            (Array.to_list @@ Array.init 6 (fun j ->
              if is_cf i (j+1) then vdd else s.t.(j)))
          else empty)
        }
      }
    in
    [
      "t1", cf 1;
      "t2", cf 2;
      "t3", cf 3;
      "t4", cf 4;
      "t5", cf 5;
      "t6", cf 6;
    ]

  let r_spec = { Signal.Seq.r_sync with Signal.Types.reg_enable = enable }

  (* state register initial values *)
  let clear _ = S.(map (fun (_,b) -> zero b) t)
  let output _ s = s
  let sched_opt = [ `cf; `me; `sc ]
  let me_rules = [ ]
end

module X = Make(PartSched)


