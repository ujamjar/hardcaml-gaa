(* MRL grammar, Figure 3-1, p37 *)

type module_defn = 
  {
    mdefn_name : string;
    mdefn_module_instances : module_instance list;
    (*mdefn_local_bindings : local_binding list;*)
    mdefn_read_methods : read_method list;
    mdefn_action_methods : action_method list;
    mdefn_rules : rule list;
  }

and module_instance = 
  {
    minst_defn_name : string;
    minst_inst_name : string;
  }

(*and local_binding = (* XXX not really sure what to do about these? *)
  {
    lbind_variable : variable;
    lbind_expr : exp;
  }*)

and read_method = 
  {
    rmeth_name : string;
    rmeth_params : variable list;
    rmeth_return : exp;
    rmeth_guard : exp;
  }


and action_method = 
  {
    ameth_name : string;
    ameth_params : variable list;
    ameth_action : action list;
    ameth_guard : exp;
  }

and variable = string

and rule = 
  {
    rule_name : string;
    rule_guard : exp;
    rule_action : action list;
  }

and exp = 
  | Econstant of int
  | Evariable of string
  | Emethod_call of method_call (* read method *)
  | Eprim_op2 of exp * prim_op * exp
  | Eprim_op1 of prim_op * exp
  | Emux2 of exp * exp * exp (* e0 ? e1 : e2 *)
  | Ewhen of exp * exp (* e when p *)
  (*| Elocal_binding of local_binding * exp*)

and prim_op = string (* for now *)

and action = 
  | Amethod_call of method_call (* action method *)
  | Aif_else of exp * action list * action list
  | Awhen of action list * exp (* a when p *)
  (*| Alocal_binding of local_binding * exp*)

and method_call = 
  {
    mcall_inst_name : string;
    mcall_meth_name : string;
    mcall_params : exp list;
  }

type scheduling_annotation = 
  | CF
  | ME
  | L
  | G
  | P
  | EXT
  | Lr
  | Gr
  | C

type scheduling_annotations = string array * scheduling_annotation array array

type primitive_module = module_defn * scheduling_annotations

(********************************************************************************)
(* simple dsl *)

let (@?) e p = Ewhen(e, p)
let mux2 c t f = Emux2(c,t,f)

let const i = Econstant i
let _false = const 0
let _true = const 1
let var v = Evariable v

let call mcall_inst_name mcall_meth_name mcall_params =
  { mcall_inst_name; mcall_meth_name; mcall_params }

let ecall i m p = Emethod_call (call i m p)

(* some basic boolean operations, for now *)
let op1 op a = Eprim_op1(op, a)
let op2 a op b = Eprim_op2(a, op, b)
let (&:) a b = op2 a "&" b
let (|:) a b = op2 a "|" b
let (^:) a b = op2 a "^" b
let (~:) a = op1 "~" a

let ($?) e p = Awhen(e, p)
let _if c t f = Aif_else(c, t, f)
let acall i m p = Amethod_call (call i m p)

let inst minst_defn_name minst_inst_name = 
  { minst_defn_name; minst_inst_name }

let rule rule_name ?(guard=_true) rule_action = 
  { rule_name; rule_guard=guard; rule_action }

let rmethod rmeth_name ?(params=[]) ?(guard=_true) rmeth_return = 
  { rmeth_name; rmeth_params=params; rmeth_guard=guard; rmeth_return }

let amethod ameth_name ?(params=[]) ?(guard=_true) ameth_action = 
  { ameth_name; ameth_params=params; ameth_guard=guard; ameth_action }

let _module
  ?(insts=[])
  ?(reads=[])
  ?(actions=[])
  ?(rules=[])
  name
  =
  {
    mdefn_name = name;
    mdefn_module_instances = insts;
    mdefn_read_methods = reads;
    mdefn_action_methods = actions;
    mdefn_rules = rules;
  }

(********************************************************************************)
(* lift whens. Figure 3-10, p50 
   - can guards have guards?  Does that make sense? 
*)

let (&:?) a = function
  | None -> a
  | Some(b) -> a &: b

let rec when_lift_exp e = 
  match e with
  (* when lifting *)
  | Ewhen(Ewhen(e, p1), p2) -> when_lift_exp e @? (p1 &: p2)
  | Eprim_op2(Ewhen(e1, p), op, e2) 
  | Eprim_op2(e1, op, Ewhen(e2, p)) -> op2 (when_lift_exp e1) op (when_lift_exp e2) @? p
  | Eprim_op1(op, Ewhen(e, p)) -> op1 op (when_lift_exp e) @? p
  | Emux2(Ewhen(c, p), t, f) -> mux2 (when_lift_exp c) (when_lift_exp t) (when_lift_exp f) @? p
  | Emethod_call meth ->
    let pred, params = when_lift_exps meth.mcall_params in
    (match pred with
    | None -> Emethod_call meth
    | Some(p) -> Emethod_call({ meth with mcall_params=params }) @? p)
  (* other cases *)
  | Econstant _ | Evariable _ -> e
  | Eprim_op2(e1, op, e2) -> op2 (when_lift_exp e1) op (when_lift_exp e2)
  | Eprim_op1(op, e) -> op1 op (when_lift_exp e)
  | Emux2(c, t, f) -> mux2 (when_lift_exp c) (when_lift_exp t) (when_lift_exp f)
  | Ewhen(e, p) -> when_lift_exp e @? p

and when_lift_exps exps = 
  let rec f pred exps = function
    | [] -> pred, List.rev exps
    | Ewhen(e, p) :: t -> f (Some(p &:? pred)) (when_lift_exp e :: exps) t
    | h :: t -> f pred (when_lift_exp h :: exps) t
  in
  f None [] exps

and when_lift_action a = 
  match a with
  (* when lifting *)
  | Amethod_call meth -> 
    let pred, params = when_lift_exps meth.mcall_params in
    (match pred with
    | None -> Amethod_call meth
    | Some(p) -> [Amethod_call({ meth with mcall_params=params })] $? p)
  | Aif_else(Ewhen(c, p), t, f) ->
    let pt, t = when_lift_actions t in
    let pf, f = when_lift_actions f in
    [Aif_else(when_lift_exp c, t, f)] $? (p &:? pt &:? pf)
  | Awhen(a, p) -> 
    let pa, a = when_lift_actions a in
    a $? (p &:? pa)
  (* other cases *)
  | Aif_else(c, t, f) ->
    let pt, t = when_lift_actions t in
    let pf, f = when_lift_actions f in
    let a = Aif_else(when_lift_exp c, t, f) in
    match pt, pf with
    | None, None -> a
    | Some(p), None | None, Some(p) -> [a] $? p
    | Some(p0), Some(p1) -> [a] $? (p0 &: p1)

and when_lift_actions acts = 
  let rec f pred acts = function
    | [] -> pred, List.rev acts
    | Awhen(a, p) :: t -> 
      let p', a' = when_lift_actions a in
      f (Some(p &:? p' &:? pred)) (a' @ acts) t (* XXX not sure... *)
    | h :: t -> f pred (when_lift_action h :: acts) t
  in
  f None [] acts

(********************************************************************************)
(* pretty print to string *)

let pretty m = 
  let buffer = Buffer.create 1024 in
  let print s = Buffer.add_string buffer s in
 
  let iter n f l = 
    if l <> [] then begin
      print ("  (* " ^ n ^ " *)\n");
      List.iter f l;
    end
  in

  let print_module_instance i = 
    print ("  " ^ i.minst_defn_name ^ " " ^ i.minst_inst_name ^ ";\n")
  in

  let rec print_sep sep f = function
    | [] -> ()
    | [x] -> f x
    | x::t -> f x; print sep; print_sep sep f t
  in

  let rec print_exp = function
    | Econstant i -> print (string_of_int i)
    | Evariable v -> print v
    | Emethod_call m ->
      print m.mcall_inst_name; print "."; print m.mcall_meth_name;
      print "(";
      print_sep ", " print_exp m.mcall_params;
      print ")"
    | Eprim_op2(e1, op, e2) ->
      print "("; print_exp e1; print (" " ^ op ^ " "); print_exp e2; print ")"
    | Eprim_op1(op, e) ->
      print "("; print (op ^ " "); print_exp e; print ")"
    | Emux2(c, t, f) ->
      print "("; print_exp c; print " ? "; print_exp t; print " : "; print_exp f; print ")"
    | Ewhen(e, p) ->
      print "("; print_exp e; print " when "; print_exp p; print ")"
  in

  let rec print_action ind = function
    | Amethod_call m ->
      print ind; print m.mcall_inst_name; print "."; print m.mcall_meth_name;
      print "(";
      print_sep ", " print_exp m.mcall_params;
      print ")\n"
    | Aif_else(c, t, f) ->
      print ind; print "if ("; print_exp c; print ") {\n";
      List.iter (print_action ("  " ^ ind)) t;
      print ind; print "} else {\n";
      List.iter (print_action ("  " ^ ind)) f;
      print ind; print "}\n";
    | Awhen(a, p) ->
      print ind; print "{\n";
      List.iter (print_action ("  " ^ ind)) a;
      print ind; print "} when "; print_exp p;
      print ";\n"
  in

  let print_rmethod m = 
    print ("  read_method " ^ m.rmeth_name ^ "(");
    List.iter (fun n -> print n; print ", ") m.rmeth_params;
    print ")\n";
    print ("    "); print_exp m.rmeth_guard; print " ==> {\n";
    print "      return "; print_exp m.rmeth_return; print ";\n";
    print "    }\n\n"
  in

  let print_amethod m = 
    print ("  action_method " ^ m.ameth_name ^ "(");
    List.iter (fun n -> print n; print ", ") m.ameth_params;
    print ")\n";
    print ("    "); print_exp m.ameth_guard; print " ==> {\n";
    List.iter (print_action "      ") m.ameth_action;
    print "    }\n\n"
  in

  let print_rule r = 
    print ("  rule " ^ r.rule_name ^ "\n");
    print ("    "); print_exp r.rule_guard; print " ==> {\n";
    List.iter (print_action "      ") r.rule_action;
    print "    }\n\n"
  in

  print ("module " ^ m.mdefn_name ^ " {\n");
  iter "instances" print_module_instance m.mdefn_module_instances;
  iter "read methods" print_rmethod m.mdefn_read_methods;
  iter "action methods" print_amethod m.mdefn_action_methods;
  iter "rules" print_rule m.mdefn_rules;
  print "}\n";
  Buffer.contents buffer

(* primitive register *)

let mkREG = 
  _module 
    ~reads:[ rmethod "read" _false ]
    ~actions:[ amethod "write" ~params:["data_in"] [] ]
    "mkREG",
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
        wr "data1" (var "x");
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

