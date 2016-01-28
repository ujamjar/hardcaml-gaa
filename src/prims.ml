open Mrl.Types
open Mrl.Dsl

let mkREG = 
  _module 
    ~reads:[ rmethod "read" _false ]
    ~actions:[ amethod "write" ~params:["data_in"] [] ]
    "mkREG"

let mkREG_sched = 
  ([|"read"; "write"|],
  [|
    [| CF; L   |];
    [| G ; EXT |];
  |])

(* XXX need some sort of interface scheme here *)
let mkREG_rtl data_in data_in_en = 
  let open HardCaml.Signal.Comb in
  let open HardCaml.Signal.Seq in
  let data_in_rdy = vdd in
  let data_out = reg r_sync data_in_en data_in in
  let data_out_rdy = vdd in
  data_in_rdy, data_out, data_out_rdy

let rd n = ecall n "read" []
let wr n d = acall n "write" [ d ]

let mkFIFO = 
  _module
    ~insts:[
      inst "mkREG" "data0";
      inst "mkREG" "data1";
      inst "mkREG" "full0";
      inst "mkREG" "full1";
    ]
    ~reads:[
      rmethod "first" ~guard:(rd "full0") (rd "data0") 
    ]
    ~actions:[
      amethod "enq" ~params:["x"] ~guard:(~: (rd "full0")) [
        wr "data1" (var "x");      (* XXX why 'x' and not 'data0'? *)
        wr "full1" (rd "full0");
        _if ( ~: (rd "full0") ) [
          wr "data0" (var "x");
         ] [];
        wr "full0" _true;
      ];
      amethod "deq" ~guard:(rd "full0") [
        wr "full1" _false;
        wr "full0" (rd "full1");
        wr "data0" (rd "data1");
      ];
      amethod "clear" [
        wr "full1" (const 0);
        wr "full0" (const 0);
      ];
    ]
    ~rules:[ ]
    "mkFIFO"


