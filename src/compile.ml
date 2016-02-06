open HardCaml.Signal.Comb

module Int = struct type t = int let compare = compare end
module IntSet = Set.Make(Int)

module Encoder = struct

  type t = [ `priority of int list | `enum of int list * int list array ]

  let asserted_bits size idx = 
    let rec f n = 
      if n=size then []
      else if (idx land (1 lsl n)) <> 0 then n :: f (n+1)
      else f (n+1)
    in
    f 0

  (* find largest clique.  Break ties with lowest enabled index *)
  let cmp_clique_largest ((size1,min1),_) ((size2,min2),_) = 
    let c = - (compare size1 size2) in
    if c=0 then compare min1 min2 else c

  (* find largest clique which includes lowest enabled index *)
  let cmp_clique_pri ((size1,min1),_) ((size2,min2),_) = 
    let c = compare min1 min2 in
    if c=0 then - (compare size1 size2) else c

  (* XXX do a bit more testing on this *)
  let max_clique cmp g =
    let m = Sched.Graph.cliques g in
    let rec minl x = function [] -> x | h::t -> minl (min x h) t in 
    let m = List.map (fun m -> (List.length m, minl (-1) m), m) m in
    let m = List.sort cmp m in
    assert (List.length m > 0);
    snd (List.hd m)

  let enumerated_encoder rules constraints group =
    let open Sched.Constraints in
    (* get rules and constraints in group *)
    let get_group rules constraints group = 
      let rules = List.map (fun i -> i, rules.(i)) group in
      let set = List.fold_left (fun s (i,_) -> IntSet.add i s) IntSet.empty rules in
      let constraints =
        let cond c = IntSet.mem c.i1 set && IntSet.mem c.i2 set && is_parallel c in
        List.filter cond constraints 
      in
      Array.of_list rules, constraints
    in
    (* focus on this scheduling group *)
    let rules, constraints = get_group rules constraints group in 
    let n_rules = Array.length rules in
    if constraints = [] then `priority group
    else 
      (* find cliques *)
      `enum (group, 
        Array.init (1 lsl n_rules) 
          (fun i ->
            (* enabled subgroup *)
            let group = asserted_bits n_rules i in
            let rules, constraints = get_group rules constraints group in
            let g = Sched.Graph.build_graph (Array.length rules) constraints (fun _ -> true) in
            max_clique cmp_clique_pri g))

  let const_of_clique gr cl = 
    let module B = HardCaml.Bits.Comb.IntbitsList in
    let size = List.length gr in
    constibl @@ 
      B.(reduce (|:) @@ 
        List.mapi (fun i idx -> if List.mem idx cl then sll (one size) i else zero size) gr)
   
  let pri_en = 
    let rec pri set = function
      | [] -> []
      | h::t -> 
        let b = h &: (~: set) in
        b :: pri b t
    in
    pri gnd

  let pri_en_tree x =  
    let rec pri1 = function 
      | [] -> [] 
      | [a] -> [[a],a] 
      | a::b::t -> 
        let a,b = a, (b &: (~: a)) in 
        ([a;b],(a|:b)) :: pri1 t 
    in
    let pri2 = function 
      | [a] -> a 
      | [(a,b);(c,d)] -> (a @ List.map (fun c -> c &: ~: b) c), b |: d 
      | _ -> failwith "bad pri tree"
    in
    try fst @@ tree 2 pri2 (pri1 x) with _ -> failwith "pri_en_tree"

  let pri_mux l = 
    tree 2 
      (function [e,a] -> e, a
              | [(e1,a1);(e2,a2)] -> e1 |: e2, mux2 e1 a1 a2
              | _ -> failwith "bad pri_mux tree") l

end

module Build = struct

  open HardCaml.Signal.Types
  open HardCaml.Signal.Comb
  open State

  let (--) s n = 
    let w = wire (width s) in
    w <== s;
    w -- n

  let state ~st_spec = 
    let s = State.map_t (fun (n,b) -> n, wire b) st_spec in
    let map = State.fold_left (fun m (_,w) -> UidMap.add (uid w) w m) UidMap.empty s in
    s, map

  (* XXX could mangle the names, but we'll address this properly when we 
   * consider module hierarchies *)
  let check_rule_names_unique rules = 
    let f (a,_) = a in
    if List.length rules <> 
      (Sched.StrSet.cardinal (Sched.StrSet.of_list (List.map f rules))) then begin
      failwith "rule and method names must be unique"
    end

  let check_rule_state s (rule_name,rule) = 
    try State.iter2 (fun (n,_) (m,_) -> assert (n=m)) s rule.Rule.action
    with _ -> failwith ("invalid state in rule " ^ rule_name)

  let method_rules ~methods ~i ~st = 
    let open Rule in
    let open Method in
    let meth (n,m) = 
      let rule, rets = m.spec.fn i st m.args in
      (n, {rule with guard = m.en}), (rule.guard, rets)
    in
    let r = List.map meth methods in
    List.map fst r, List.map snd r

  let instantiate_rules ~methods ~rules ~i ~s =
    let state = state s in
    let methods, method_outputs = method_rules methods i (fst state) in
    let rules = List.map (fun (n,r) -> n, r ~i ~s:(fst state)) rules in
    let rules = List.map (fun (n,r) -> n,r) (methods @ rules) in
    check_rule_names_unique rules;
    List.iter (check_rule_state s) rules;
    state, rules, method_outputs

  let rule_enables ~rules ~sched = 
    let get_enables gr = 
      List.map (fun i -> let n,r = Array.get rules i in r.Rule.guard -- ("__"^n^"_g")) gr 
    in
    try
      List.map
        (fun s ->
          match s with
          | `priority gr -> 
            List.map2 (fun idx en -> idx,en) gr (Encoder.pri_en_tree @@ get_enables gr)
          | `enum (gr,cl) -> 
            let sel = concat @@ List.rev @@ get_enables gr in
            let data = List.map (Encoder.const_of_clique gr) @@ Array.to_list cl in
            let en = List.rev @@ bits @@ mux sel data in
            List.map2 (fun idx en -> idx,en) gr en)
        sched
    with _ -> failwith "rule enables"

  let sort_guards guards = 
    Array.of_list @@ List.sort (fun a b -> compare (fst a) (fst b)) @@ List.concat guards 

  let create_register_state ~r_spec ~st_clear ~state
    ~(rules:(string * Rule.inst) array) ~guards = 
    let state = State.to_list @@ fst state in
    let guards = sort_guards guards in
    let rules = Array.init (Array.length rules) 
      (fun i -> 
        let n,r = rules.(i) in
        let k,e = guards.(i) in
        assert (i=k);
        n, r, e -- ("__"^n^"_e"))
    in
    let any_enabled = reduce (|:) @@ Array.to_list @@ Array.map (fun (_,_,e) -> e) rules in

    let merge_st st a e = 
      List.(map2 (fun st (_,a) -> if a = Signal_empty then st else (e,a)::st) st a)
    in
    let st_mux = (* state updates in reverse priority order *)
      Array.fold_left (fun st (n,r,e) -> merge_st st (State.to_list r.Rule.action) e) 
        (List.map (fun _ -> []) state)
        rules
    in
    let st_reg = 
      let st = List.map2 (fun s (_,t) -> List.rev s,t) st_mux (State.to_list st_clear) in
      List.(map2 
        (fun (n,_) (st,stc) -> 
          if st=[] then n, stc 
          else
            let e, a = Encoder.pri_mux st in
            let r_spec = { r_spec with reg_clear_value=stc; reg_reset_value=stc } in
            n, HardCaml.Signal.Seq.(reg r_spec e a) -- n) state st) 
    in

    ignore @@ (List.map2 (fun (_,l) (_,r) -> l <== r) state st_reg);
    any_enabled, State.of_list st_reg

  let schedule
    ?(sched_opt=[`cf; `me; `sc])
    ?(me_rules=[])
    ~methods ~rules ~i ~s = 
    let state, rules, method_outputs = instantiate_rules ~methods ~rules ~i ~s in
    let dnr = Sched.DnR.make state rules in
    let pairs = Sched.pairs (List.mapi (fun i (n,_) -> n,i) rules) in
    let constraints = Sched.Constraints.make ~sched_opt ~me_rules dnr pairs in
    state, rules, method_outputs, constraints

  let print_constraints chan rules constraints = 
    let open Sched.Constraints in
    let b b = match Lazy.force b with true -> "\xe2\x9c\x93" | false -> "\xe2\x9c\x97" in
    Printf.fprintf chan "| cf | me | sc12 | sc21 |\n";
    Printf.fprintf chan "|-----------------------|\n";
    List.iter (fun c ->
      Printf.fprintf chan "| %s  | %s  | %s    |  %s   | %s -> %s\n" 
        (b c.cf) (b c.me) (b c.sc12) (b c.sc21)
        (fst (List.nth rules c.i1))
        (fst (List.nth rules c.i2))
    ) constraints;
    Printf.fprintf chan "|-----------------------|%!\n"

  let print_schedule chan rules sched = 
    List.iteri (fun i g ->
      Printf.fprintf chan "group %i\n" i;
      List.iter (fun g -> Printf.fprintf chan "%s[%i] " (fst (List.nth rules g)) g) g;
      Printf.fprintf chan "\n%!") sched

  let compile 
    ?(sched_opt=[`cf; `me; `sc])
    ?(me_rules=[])
    ~r_spec ~st_clear ~methods ~rules ~i ~s = 
    let state, rules, method_outputs, constraints = 
      schedule ~sched_opt ~me_rules ~methods ~rules ~i ~s
    in
    print_constraints stdout rules constraints;
    let scheduling_groups = Sched.Graph.conflict_graph (List.length rules) constraints in
    print_schedule stdout rules scheduling_groups;
    let rules = Array.of_list rules in
    let encoders = List.map (Encoder.enumerated_encoder rules constraints) scheduling_groups in
    let guards = rule_enables rules encoders in
    let regst = create_register_state r_spec st_clear state rules guards in
    regst, method_outputs

end

