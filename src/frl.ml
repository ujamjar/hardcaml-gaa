(* Flat Rule Language *)

module Types = struct

  type module_defn = 
    {
      mdefn_name : string;
      mdefn_prims : primitive list;
      mdefn_rules : rule list;
    }

  and primitive = string (* just a register for now *)

  and exp = 
    | Econstant of int
    | Emethod_call of method_call (* read method *)
    | Eprim_op2 of exp * prim_op * exp
    | Eprim_op1 of prim_op * exp
    | Emux2 of exp * exp * exp (* e0 ? e1 : e2 *)

  and prim_op = string (* for now *)

  and method_call = 
    {
      mcall_inst_name : string;
      mcall_meth_name : string;
      mcall_params : exp list;
    }

  and rule = 
    {
      rule_name : string;
      rule_guard : exp;
      rule_action : action list;
    }

  and action = 
    | Amethod_call of method_call (* action method *)
    | Aif_else of exp * action list * action list

end

module Dsl = struct

  open Types

  let mux2 c t f = Emux2(c,t,f)

  let const i = Econstant i
  let _false = const 0
  let _true = const 1

  let call mcall_inst_name mcall_meth_name mcall_params =
    { mcall_inst_name; mcall_meth_name; mcall_params }

  let ecall i m p = Emethod_call (call i m p)

  let op1 op a = Eprim_op1(op, a)
  let op2 a op b = Eprim_op2(a, op, b)
  let (&:) a b = op2 a "&" b
  let (|:) a b = op2 a "|" b
  let (^:) a b = op2 a "^" b
  let (~:) a = op1 "~" a

  let (+:) a b = op2 a "+" b
  let (-:) a b = op2 a "-" b

  let (<:)  a b = op2 a "<"  b
  let (<=:) a b = op2 a "<=" b
  let (>:)  a b = op2 a ">"  b
  let (>=:) a b = op2 a ">=" b
  let (==:) a b = op2 a "==" b
  let (<>:) a b = op2 a "<>" b

  let _if c t f = Aif_else(c, t, f)
  let acall i m p = Amethod_call (call i m p)

  let reg name = 
    object
      method get = ecall name "get" []
      method set e = acall name "set" [ e ]
      method prim = name
    end

  let rule rule_name ?(guard=_true) rule_action = 
    { rule_name; rule_guard=guard; rule_action }

  let _module ~prims ~rules name
    =
    {
      mdefn_name = name;
      mdefn_prims = prims;
      mdefn_rules = rules;
    }

end

module Pretty = struct

  open Types

  let to_string m = 
    let buffer = Buffer.create 1024 in
    let print s = Buffer.add_string buffer s in
  
    let iter n f l = 
      if l <> [] then begin
        print ("  (* " ^ n ^ " *)\n");
        List.iter f l;
      end
    in

    let rec print_sep sep f = function
      | [] -> ()
      | [x] -> f x
      | x::t -> f x; print sep; print_sep sep f t
    in

    let rec print_exp = function
      | Econstant i -> print (string_of_int i)
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
    in

    let rec print_action ind = function
      | Amethod_call m ->
        print ind; print m.mcall_inst_name; print "."; print m.mcall_meth_name;
        print "(";
        print_sep ", " print_exp m.mcall_params;
        print ");\n"
      | Aif_else(c, t, f) ->
        print ind; print "if ("; print_exp c; print ") {\n";
        List.iter (print_action ("  " ^ ind)) t;
        print ind; print "} else {\n";
        List.iter (print_action ("  " ^ ind)) f;
        print ind; print "}\n";
    in

    let print_rule r = 
      print ("  rule " ^ r.rule_name ^ "\n");
      print ("    "); print_exp r.rule_guard; print " ==> {\n";
      List.iter (print_action "      ") r.rule_action;
      print "    }\n\n"
    in

    print ("module " ^ m.mdefn_name ^ " {\n");
    iter "rules" print_rule m.mdefn_rules;
    print "}\n";
    Buffer.contents buffer


end

module Eval = struct

  open Types


  let op2 = [
    "&", (land); "|", (lor); "^", (lxor);
    "+", (+); "-", (-);
    "<",  (fun a b -> if a <  b then 1 else 0);
    "<=", (fun a b -> if a <= b then 1 else 0);
    ">",  (fun a b -> if a >  b then 1 else 0);
    ">=", (fun a b -> if a >= b then 1 else 0);
    "==", (fun a b -> if a == b then 1 else 0);
    "<>", (fun a b -> if a <> b then 1 else 0);
  ] 

  let op1 = [ "~", (lnot) ] 

  let run ?(steps=0) m = 

    let prims = List.map (fun n -> n, ref 0) m.mdefn_prims in
    let prims_next = List.map (fun n -> n, ref 0) m.mdefn_prims in

    let return_prims () = List.map (fun (n,b) -> n,!b) prims in

    let rec exp e = 
      match e with
      | Econstant i -> i
      | Emethod_call m -> begin
        match m.mcall_meth_name with
        | "get" -> !(List.assoc m.mcall_inst_name prims)
        | _ -> failwith ("Emethod_call: " ^ m.mcall_meth_name)
      end
      | Eprim_op2(e0, op, e1) -> (List.assoc op op2) (exp e0) (exp e1)
      | Eprim_op1(op, e) -> (List.assoc op op1) (exp e)
      | Emux2(s,t,f) -> if exp s = 0 then exp f else exp t
    in

    (* evaluate rules predicate *)
    let guard rule = exp rule.rule_guard <> 0 in

    (* run rules action *)
    let rec action a =
      List.iter (function
        | Amethod_call m -> begin
          match m.mcall_meth_name with
          | "set" -> begin
            let x = List.assoc m.mcall_inst_name prims_next in
            let arg = exp (List.hd m.mcall_params) in
            x := arg
          end
          | _ -> failwith ("Amethod_call: " ^ m.mcall_meth_name)
        end
        | Aif_else(s, ta, fa) -> begin
          if exp s <> 0 then action ta
          else action fa
        end) a;
    in

    (* after an action runs, update the state *)
    let update_state () = List.iter2 (fun (_,a) (_,b) -> a := !b) prims prims_next in

    (* evaluate rule predicate, and if true run it's action *)
    let rule rule = 
      if guard rule then 
        (Printf.printf "%s\n" rule.rule_name; action rule.rule_action; update_state(); true)
      else false
    in

    (* run for given number of steps, or until no rule predicate fires *)
    let rec run step = 
      if steps<>0 && step >= steps then return_prims ()
      else 
        let run_rules () = List.fold_left (fun b r -> b || rule r) false m.mdefn_rules in
        if run_rules () then run (step + 1)
        else return_prims ()
    in

    run 0


end

module Gcd = struct

  open Dsl

  let gcd = 
    let run = reg "run" in
    let x, y = reg "x", reg "y" in
    _module "gcd"
      ~prims:[ run#prim; x#prim; y#prim ]
      ~rules:[
        rule "init" ~guard:(run#get ==: const 0) [
          run#set (const 1);
          x#set @@ const 15;
          y#set @@ const 6;
        ];
        rule "sub" ~guard:((x#get >=: y#get) &: (y#get <>: const 0) &: run#get) [
          x#set (x#get -: y#get);
        ];
        rule "swap" ~guard:((x#get <: y#get) &: (y#get <>: const 0) &: run#get) [
          x#set y#get;
          y#set x#get;
        ];
      ]

end


