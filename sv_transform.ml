(* ============================================================================
   sv_transform.ml - Transformation pass to convert non-synth to synth
   ============================================================================ *)

open Sv_ast

(* ============================================================================
   CONFIGURATION AND STATISTICS
   ============================================================================ *)

let max_unroll_iterations = 1024
let debug = ref false

type transform_stats = {
  mutable loops_unrolled: int;
  mutable functions_inlined: int;
  mutable tasks_inlined: int;
}

let stats = {
  loops_unrolled = 0;
  functions_inlined = 0;
  tasks_inlined = 0;
}

(* ============================================================================
   SSA CONVERSION
   ============================================================================ *)

type ssa_context = {
  versions: (string, int) Hashtbl.t;
  mutable temp_counter: int;
  stem: string;  (* used for Begin-based naming *)
  new_decls: (string, sv_node) Hashtbl.t;  (* track new SSA vars *)
}

let create_ssa_context ?(stem="") () = {
  versions = Hashtbl.create 50;
  temp_counter = 0;
  stem;
  new_decls = Hashtbl.create 50;
}

let get_version ctx var_name =
  try Hashtbl.find ctx.versions var_name
  with Not_found -> 0

(* Create new SSA version, record declaration if needed *)
let new_version ctx var_name ~dtype =
  let current = get_version ctx var_name in
  let next = current + 1 in
  Hashtbl.replace ctx.versions var_name next;
  let new_name =
    if ctx.stem = "" then Printf.sprintf "%s_%d" var_name next
    else Printf.sprintf "%s_%s_%d" ctx.stem var_name next
  in
  (if not (Hashtbl.mem ctx.new_decls new_name) then
    let decl = Var {
      name = new_name;
      dtype_ref = dtype;
      var_type = "TEMP";
      direction = "";
      value = None;
      dtype_name = "";
      is_param = false
    } in
    Hashtbl.add ctx.new_decls new_name decl);
  new_name

let get_versioned_name ctx var_name =
  let version = get_version ctx var_name in
  if version = 0 then var_name
  else Printf.sprintf "%s_%d" var_name version

(* Flatten Begin blocks *)
let rec flatten_begins stmts =
  List.concat_map (function
    | Begin { stmts = inner; _ } -> flatten_begins inner
    | stmt -> [stmt]
  ) stmts

(* Detect multi-assigned variables in blocking assignments *)
let rec find_multi_assigned stmts =
  let assignments = Hashtbl.create 20 in
  let rec count stmt =
    match stmt with
    | Assign { lhs = VarRef { name; access; _ }; is_blocking = true; _ } ->
        let c = try Hashtbl.find assignments name with Not_found -> 0 in
        Hashtbl.replace assignments name (c + 1)
    | Begin { stmts; _ } | Always { stmts; _ } ->
        List.iter count stmts
    | If { then_stmt; else_stmt; _ } ->
        count then_stmt;
        Option.iter count else_stmt
    | _ -> ()
  in
  List.iter count (flatten_begins stmts);
  Hashtbl.fold (fun k v acc -> if v > 1 then k :: acc else acc) assignments []

(* SSA expression *)
let rec ssa_expr ctx expr =
  match expr with
  | VarRef { name; access = "RD"; dtype_ref } ->
      VarRef { name = get_versioned_name ctx name; access = "RD"; dtype_ref }
  | VarRef _ -> expr
  | BinaryOp { op; lhs; rhs } ->
      BinaryOp { op; lhs = ssa_expr ctx lhs; rhs = ssa_expr ctx rhs }
  | UnaryOp { op; operand } ->
      UnaryOp { op; operand = ssa_expr ctx operand }
  | Cond { condition; then_val; else_val } ->
      Cond {
        condition = ssa_expr ctx condition;
        then_val = ssa_expr ctx then_val;
        else_val = ssa_expr ctx else_val;
      }
  | Sel { expr; lsb; width; range } ->
      Sel {
        expr = ssa_expr ctx expr;
        lsb = Option.map (ssa_expr ctx) lsb;
        width = Option.map (ssa_expr ctx) width;
        range;
      }
  | ArraySel { expr; index } ->
      ArraySel { expr = ssa_expr ctx expr; index = ssa_expr ctx index }
  | Concat { parts } ->
      Concat { parts = List.map (ssa_expr ctx) parts }
  | Replicate { src; count; dtype_ref } ->
      Replicate { src = ssa_expr ctx src; count = ssa_expr ctx count; dtype_ref }
  | _ -> expr

(* SSA statement with new_decls tracking *)
let rec ssa_stmt ctx multi stmt =
  match stmt with
  | Assign { lhs = VarRef { name; access; dtype_ref }; rhs; is_blocking = true } ->
      let rhs_ssa = ssa_expr ctx rhs in
      if List.mem name multi then
        let new_name = new_version ctx name ~dtype:dtype_ref in
        Assign { lhs = VarRef { name = new_name; access; dtype_ref }; rhs = rhs_ssa; is_blocking = true }
      else Assign { lhs = VarRef { name; access; dtype_ref }; rhs = rhs_ssa; is_blocking = true }

  | Assign { lhs; rhs; is_blocking } ->
      Assign { lhs = ssa_expr ctx lhs; rhs = ssa_expr ctx rhs; is_blocking }

  | AssignW { lhs; rhs } ->
      AssignW { lhs = ssa_expr ctx lhs; rhs = ssa_expr ctx rhs }

  | Begin { name; stmts; is_generate } ->
      let ctx' = { ctx with stem = if name = "" then ctx.stem else name } in
      let transformed_stmts = List.map (ssa_stmt ctx' multi) stmts in
      Hashtbl.iter (fun k v -> if not (Hashtbl.mem ctx.new_decls k) then Hashtbl.add ctx.new_decls k v) ctx'.new_decls;
      (* prepend declarations for this block *)
      let decls = Hashtbl.fold (fun _ v acc -> v :: acc) ctx'.new_decls [] in
      Begin { name; stmts = decls @ transformed_stmts; is_generate }

  | If { condition; then_stmt; else_stmt } ->
      If {
        condition = ssa_expr ctx condition;
        then_stmt = ssa_stmt ctx multi then_stmt;
        else_stmt = Option.map (ssa_stmt ctx multi) else_stmt;
      }

  | Always { always; senses; stmts } ->
      Always { always; senses; stmts = List.map (ssa_stmt ctx multi) stmts }

  | _ -> stmt

let convert_to_ssa stmts =
  let flat = flatten_begins stmts in
  let multi = find_multi_assigned flat in
  if multi = [] then stmts
  else
    let ctx = create_ssa_context () in
    let ssa_stmts = List.map (ssa_stmt ctx multi) flat in
    let decls = Hashtbl.fold (fun _ v acc -> v :: acc) ctx.new_decls [] in
    decls @ ssa_stmts

(* ============================================================================
   LOOP UNROLLING
   ============================================================================ *)

(* Substitute a variable by a constant in expressions *)
let rec substitute_var var_name value expr =
  match expr with
  | VarRef { name; access; dtype_ref } when name = var_name ->
      Const { name = string_of_int value; dtype_ref = None }
  | BinaryOp { op; lhs; rhs } ->
      BinaryOp { op; lhs = substitute_var var_name value lhs; rhs = substitute_var var_name value rhs }
  | UnaryOp { op; operand } ->
      UnaryOp { op; operand = substitute_var var_name value operand }
  | Cond { condition; then_val; else_val } ->
      Cond {
        condition = substitute_var var_name value condition;
        then_val = substitute_var var_name value then_val;
        else_val = substitute_var var_name value else_val;
      }
  | Sel { expr; lsb; width; range } ->
      Sel {
        expr = substitute_var var_name value expr;
        lsb = Option.map (substitute_var var_name value) lsb;
        width = Option.map (substitute_var var_name value) width;
        range;
      }
  | ArraySel { expr; index } ->
      ArraySel { expr = substitute_var var_name value expr; index = substitute_var var_name value index }
  | Concat { parts } ->
      Concat { parts = List.map (substitute_var var_name value) parts }
  | Replicate { src; count; dtype_ref } ->
      Replicate { src = substitute_var var_name value src; count = substitute_var var_name value count; dtype_ref }
  | _ -> expr

let rec substitute_var_stmt var_name value stmt =
  match stmt with
  | Assign { lhs; rhs; is_blocking } ->
      Assign { lhs = substitute_var var_name value lhs; rhs = substitute_var var_name value rhs; is_blocking }
  | AssignW { lhs; rhs } ->
      AssignW { lhs = substitute_var var_name value lhs; rhs = substitute_var var_name value rhs }
  | If { condition; then_stmt; else_stmt } ->
      If { condition = substitute_var var_name value condition;
           then_stmt = substitute_var_stmt var_name value then_stmt;
           else_stmt = Option.map (substitute_var_stmt var_name value) else_stmt }
  | Begin { name; stmts; is_generate } ->
      Begin { name; stmts = List.map (substitute_var_stmt var_name value) stmts; is_generate }
  | _ -> stmt

let rec try_eval_const expr =
  match expr with
  | Const { name; _ } ->
      (* Handle Verilog-style constants with size/base prefixes *)
      (try
         if String.contains name '\'' then
           let parts = String.split_on_char '\'' name in
           match parts with
           | width :: rest ->
               let value_str = String.concat "'" rest in
               if String.length value_str >= 2 then
                 let result =
                   match value_str.[0], value_str.[1] with
                   | 's', 'h' -> int_of_string ("0x" ^ String.sub value_str 2 (String.length value_str - 2))
                   | 'h', _ -> int_of_string ("0x" ^ String.sub value_str 1 (String.length value_str - 1))
                   | 's', 'd' -> int_of_string (String.sub value_str 2 (String.length value_str - 2))
                   | 'd', _ -> int_of_string (String.sub value_str 1 (String.length value_str - 1))
                   | _ -> int_of_string value_str
                 in Some result
               else Some (int_of_string value_str)
           | _ -> Some (int_of_string name)
         else Some (int_of_string name)
       with _ -> None)

  | BinaryOp { op; lhs; rhs } ->
      (match try_eval_const lhs, try_eval_const rhs with
       | Some l, Some r ->
           (match op with
            | "ADD" | "ADDS" -> Some (l + r)
            | "SUB" | "SUBS" -> Some (l - r)
            | "MUL" | "MULS" -> Some (l * r)
            | "DIV" | "DIVS" when r <> 0 -> Some (l / r)
            | _ -> None)
       | _ -> None)

  | UnaryOp { op; operand } ->
      (match try_eval_const operand with
       | Some v ->
           (match op with
            | "NEG" | "MINUS" -> Some (-v)
            | _ -> None)
       | None -> None)

  | Cond { condition; then_val; else_val } ->
      (match try_eval_const condition with
       | Some 0 -> try_eval_const else_val
       | Some _ -> try_eval_const then_val
       | None -> None)

  | _ -> None

let extract_increment loop_var increment =
  match increment with
  | Assign { lhs = VarRef { name; _ }; rhs; _ } when name = loop_var ->
      (match rhs with
       | BinaryOp { op; lhs = VarRef { name = v; _ }; rhs = Const { name = c; _ } } when v = loop_var ->
           let c_int = try int_of_string c with _ -> 1 in
           Some (if op = "ADD" then c_int else if op = "SUB" then -c_int else 0)

       | BinaryOp { op; lhs = Const { name = c; _ }; rhs = VarRef { name = v; _ } } when v = loop_var ->
           let c_int = try int_of_string c with _ -> 1 in
           Some (if op = "ADD" then c_int else if op = "SUB" then -c_int else 0)

       | _ -> None)
  | _ -> None  

(* Unroll a For node using SSA context; returns Some stmt_list or None *)
let unroll_for_loop_ssa ctx ~loop_name ~init_stmt ~cond_expr ~step_expr ~body ~dtype_opt =
  if !debug then Printf.eprintf "Analyzing for-loop: %s\n" loop_name;

  (* start value *)
  let start_val =
    match init_stmt with
    | Assign { rhs; _ } -> try_eval_const rhs
    | _ -> None
  in

  (* increment: try step_expr first, otherwise try to infer from step Assign node *)
  let inc_amount =
    match step_expr with
    | Some e -> try_eval_const e
    | None -> None
  in

  (* If step_expr wasn't a simple const, try pattern like i = i + C (user may store step separately) *)
  let inc_amount =
    match inc_amount with
    | Some v -> Some v
    | None ->
        (match step_expr with
         | Some (BinaryOp { op; lhs = Const _ as c; rhs = VarRef { name = v; _ } })
         | Some (BinaryOp { op; lhs = VarRef { name = v; _ }; rhs = Const _ as c }) when v = loop_name ->
             (* use try_eval_const on the const side *)
             (match try_eval_const c with Some cval -> Some (if op = "ADD" then cval else if op = "SUB" then -cval else 0) | None -> None)
         | _ -> None)

  in

  (* bound / comparison *)
  let bound_val, comparison_op =
    match cond_expr with
    | BinaryOp { op; lhs; rhs } ->
        let lhs_c = try_eval_const lhs in
        let rhs_c = try_eval_const rhs in
        (match op, lhs_c, rhs_c with
         | "GTS", Some b, None -> (Some b, "GTS")
         | "LTS", None, Some b -> (Some b, "LTS")
         | "LTES", None, Some b -> (Some b, "LTES")
         | "GTES", Some b, None -> (Some b, "GTES")
         | _ -> (None, ""))
    | _ -> (None, "")
  in

  match start_val, bound_val, inc_amount with
  | Some start, Some bound, Some inc when inc <> 0 ->
      let num_iters =
        match comparison_op with
        | "GTS" -> (bound - start) / inc
        | "LTS" -> (bound - start) / inc
        | "LTES" -> ((bound - start) / inc) + 1
        | "GTES" -> ((bound - start) / inc) + 1
        | _ -> 0
      in
      if num_iters <= 0 || num_iters >= max_unroll_iterations then None
      else begin
        if !debug then Printf.eprintf "Unrolling %d iterations for %s\n" num_iters loop_name;
        let rec gen_iter i remaining acc =
          if remaining <= 0 then acc
          else
            let iteration_stmts =
              List.map (fun s ->
                 (* create SSA version(s) for LHS variables that are multi-assigned (use ctx/your multi list) *)
                 (* simplest approach: version any assignment LHS equal to original loop-carried var or variables in multi list *)
                 let new_name = new_version ctx loop_name ~dtype:dtype_opt in
                 let s_sub = substitute_var_stmt loop_name i s in
                 match s_sub with
                 | Assign { lhs = VarRef { name; access; dtype_ref }; rhs; is_blocking } when name = loop_name ->
                     Assign { lhs = VarRef { name = new_name; access; dtype_ref = dtype_opt }; rhs = ssa_expr ctx rhs; is_blocking }
                 | Assign { lhs = VarRef { name; access; dtype_ref }; rhs; is_blocking } ->
                     (* other assigns: rewrite rhs reads with ssa_expr *)
                     Assign { lhs = VarRef { name = get_versioned_name ctx name; access; dtype_ref }; rhs = ssa_expr ctx rhs; is_blocking }
                 | other -> other
              ) body
            in
            gen_iter (i + inc) (remaining - 1) (acc @ iteration_stmts)
        in
        let unrolled = gen_iter start num_iters [] in
        stats.loops_unrolled <- stats.loops_unrolled + 1;
        Some unrolled
      end
  | _ -> None

(* ============================================================================
   FUNCTION AND TASK INLINING
   ============================================================================ *)

type symbol_table = {
  functions: (string, sv_node) Hashtbl.t;
  tasks: (string, sv_node) Hashtbl.t;
}

let create_symbol_table () = { functions = Hashtbl.create 50; tasks = Hashtbl.create 50 }

let rec collect_functions_tasks symtab stmts =
  List.iter (function
    | Func { name; _ } as f -> Hashtbl.replace symtab.functions name f
    | Task { name; _ } as t -> Hashtbl.replace symtab.tasks name t
    | Begin { stmts; _ } | Always { stmts; _ } -> collect_functions_tasks symtab stmts
    | _ -> ()
  ) stmts

let inline_call symtab node =
  match node with
(*
  | CallFunc { name; args } ->
      (match Hashtbl.find_opt symtab.functions name with
      | Some (Func { stmts; params; _ }) ->
          stats.functions_inlined <- stats.functions_inlined + 1;
          (* Simple positional substitution *)
          List.map2 (fun param arg ->
            Assign { lhs = VarRef { name = param; access = "WR"; dtype_ref = None }; rhs = arg; is_blocking = true }
          ) params args @ stmts
      | None -> [node])
  | CallTask { name; args } ->
      (match Hashtbl.find_opt symtab.tasks name with
      | Some (Task { stmts; params; _ }) ->
          stats.tasks_inlined <- stats.tasks_inlined + 1;
          List.map2 (fun param arg ->
            Assign { lhs = VarRef { name = param; access = "WR"; dtype_ref = None }; rhs = arg; is_blocking = true }
          ) params args @ stmts
      | None -> [node])
*)
  | _ -> [node]

(* ============================================================================
   TRANSFORMATION PASS
   ============================================================================ *)
(* Recursively transform statements with proper loop detection and SSA naming *)
let rec transform_stmt symtab ctx ?(stem="") stmt =
  match stmt with
  | Assign { lhs; rhs; is_blocking } ->
      Assign { lhs; rhs = transform_expr symtab rhs; is_blocking }

  | AssignW { lhs; rhs } ->
      AssignW { lhs; rhs = transform_expr symtab rhs }

  | Always { always; senses; stmts } ->
      if !debug then Printf.eprintf "Processing Always block: %s\n" always;
      let transformed = List.map (transform_stmt symtab ctx) stmts in
      let ssa_stmts = convert_to_ssa transformed in
      Always { always; senses; stmts = ssa_stmts }

  | Begin { name; stmts; is_generate } ->
      let current_stem = if name <> "" then name else stem in
      Begin { name; stmts = List.map (transform_stmt symtab ctx ?stem:(Some current_stem)) stmts; is_generate }

  | For { name = loop_name; dtype_ref; lhs; rhs = init_rhs; condition; stmts = body; _ } as for_node ->
    (match unroll_for_loop_ssa ctx ~loop_name ~init_stmt:(Assign { lhs; rhs = init_rhs; is_blocking=true }) ~cond_expr:condition ~step_expr:None ~body ~dtype_opt:dtype_ref with
    | Some unrolled ->
        let decls = Hashtbl.fold (fun _ v acc -> v :: acc) ctx.new_decls [] in
        Begin { name=loop_name; stmts=unrolled; is_generate = false }
        (* append declarations then unrolled *)
	(*
         List.rev_append (List.iter (decls @ unrolled)) ctx.new_decls (* or however you push into processed list *)
	 *)
    | None -> for_node)

  | If { condition; then_stmt; else_stmt } -> (match else_stmt with
      | Some else_stmt -> If {
        condition = transform_expr symtab condition;
        then_stmt = transform_stmt symtab ctx ?stem:(Some stem) then_stmt;
        else_stmt = Some (transform_stmt symtab ctx ?stem:(Some stem) else_stmt);
      }

      | None -> If {
        condition = transform_expr symtab condition;
        then_stmt = transform_stmt symtab ctx ?stem:(Some stem) then_stmt;
        else_stmt = None
      })

  | _ -> stmt

and transform_expr symtab expr =
  match expr with
  | BinaryOp { op; lhs; rhs } ->
      BinaryOp { op; lhs = transform_expr symtab lhs; rhs = transform_expr symtab rhs }

  | UnaryOp { op; operand } ->
      UnaryOp { op; operand = transform_expr symtab operand }

  | Cond { condition; then_val; else_val } ->
      Cond {
        condition = transform_expr symtab condition;
        then_val = transform_expr symtab then_val;
        else_val = transform_expr symtab else_val;
      }

  | Sel { expr; lsb; width; range } ->
      Sel {
        expr = transform_expr symtab expr;
        lsb = Option.map (transform_expr symtab) lsb;
        width = Option.map (transform_expr symtab) width;
        range;
      }

  | ArraySel { expr; index } ->
      ArraySel { expr = transform_expr symtab expr; index = transform_expr symtab index }

  | Concat { parts } ->
      Concat { parts = List.map (transform_expr symtab) parts }

  | Replicate { src; count; dtype_ref } ->
      Replicate { src = transform_expr symtab src; count = transform_expr symtab count; dtype_ref }

  | _ -> expr

let transform_module stmts =
  let symtab = create_symbol_table () in
  let ctx = create_ssa_context () in
  collect_functions_tasks symtab stmts;
  List.map (transform_stmt symtab ctx) stmts

let rec transform_ast node =
  match node with
  | Netlist modules -> Netlist (List.map transform_ast modules)
  | Module { name; stmts } -> Module { name; stmts = transform_module stmts }
  | _ -> node

let transform ?(verbose=false) ast =
  debug := verbose;
  stats.loops_unrolled <- 0;
  stats.functions_inlined <- 0;
  stats.tasks_inlined <- 0;

  let transformed = transform_ast ast in

  if verbose then (
    Printf.printf "Loops unrolled: %d\n" stats.loops_unrolled;
    Printf.printf "Functions inlined: %d\n" stats.functions_inlined;
    Printf.printf "Tasks inlined: %d\n" stats.tasks_inlined;
  );
  transformed
