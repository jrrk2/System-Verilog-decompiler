(* ============================================================================
   sv_transform.ml - Transformation pass to convert non-synth to synth
   ============================================================================ *)

open Sv_ast

(* Configuration *)
let max_unroll_iterations = 1024
let debug = ref false

(* Statistics *)
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
   CONSTANT EXPRESSION EVALUATION
   ============================================================================ *)

(* Try to evaluate an expression to a constant - IMPROVED VERSION *)
let rec try_eval_const expr =
  let expr_type = match expr with
    | Const { name; _ } -> Printf.sprintf "Const(%s)" name
    | VarRef { name; _ } -> Printf.sprintf "VarRef(%s)" name
    | BinaryOp { op; _ } -> Printf.sprintf "BinaryOp(%s)" op
    | _ -> "Other"
  in
  if !debug then Printf.eprintf "    try_eval_const: %s\n" expr_type;
  
  match expr with
  | Const { name; _ } ->
      (* Parse constant like "32'sh0" or "32'h8" or "1'h0" *)
      (try
        if String.contains name '\'' then
          let parts = String.split_on_char '\'' name in
          match parts with
          | width :: rest ->
              let value_str = String.concat "'" rest in
              if !debug then
                Printf.eprintf "      width=%s, value=%s\n" width value_str;
              
              (* Handle different bases: 'h, 'sh, 'd, 'sd, 'b, 'sb *)
              if String.length value_str >= 2 then
                (* Check for signed hex 'sh or unsigned hex 'h *)
                if value_str.[0] = 's' && value_str.[1] = 'h' then
                  let hex = String.sub value_str 2 (String.length value_str - 2) in
                  let result = int_of_string ("0x" ^ hex) in
                  if !debug then Printf.eprintf "      => %d (signed hex)\n" result;
                  Some result
                else if value_str.[0] = 'h' then
                  let hex = String.sub value_str 1 (String.length value_str - 1) in
                  let result = int_of_string ("0x" ^ hex) in
                  if !debug then Printf.eprintf "      => %d (unsigned hex)\n" result;
                  Some result
                (* Check for signed decimal 'sd or unsigned decimal 'd *)
                else if value_str.[0] = 's' && value_str.[1] = 'd' then
                  let dec = String.sub value_str 2 (String.length value_str - 2) in
                  let result = int_of_string dec in
                  if !debug then Printf.eprintf "      => %d (signed dec)\n" result;
                  Some result
                else if value_str.[0] = 'd' then
                  let dec = String.sub value_str 1 (String.length value_str - 1) in
                  let result = int_of_string dec in
                  if !debug then Printf.eprintf "      => %d (unsigned dec)\n" result;
                  Some result
                else begin
                  if !debug then Printf.eprintf "      => None (unknown format)\n";
                  None
                end
              else begin
                if !debug then Printf.eprintf "      => None (value too short)\n";
                None
              end
          | _ -> 
              if !debug then Printf.eprintf "      => None (split failed)\n";
              None
        else begin
          (* Plain number without width/base specifier *)
          let result = int_of_string name in
          if !debug then Printf.eprintf "      => %d (plain)\n" result;
          Some result
        end
      with e ->
        if !debug then Printf.eprintf "      => Exception: %s\n" (Printexc.to_string e);
        None)
  
  | BinaryOp { op = "ADD"; lhs; rhs } ->
      (match try_eval_const lhs, try_eval_const rhs with
      | Some l, Some r -> Some (l + r)
      | _ -> None)
  
  | BinaryOp { op = "SUB"; lhs; rhs } ->
      (match try_eval_const lhs, try_eval_const rhs with
      | Some l, Some r -> Some (l - r)
      | _ -> None)
  
  | BinaryOp { op = "MUL" | "MULS"; lhs; rhs } ->
      (match try_eval_const lhs, try_eval_const rhs with
      | Some l, Some r -> Some (l * r)
      | _ -> None)
  
  | BinaryOp { op = "DIV" | "DIVS"; lhs; rhs } ->
      (match try_eval_const lhs, try_eval_const rhs with
      | Some l, Some r when r <> 0 -> Some (l / r)
      | _ -> None)
  
  | _ -> None

(* Check if a comparison is true given variable values *)
let eval_condition var_values condition =
  match condition with
  | BinaryOp { op; lhs; rhs } ->
      let lhs_val = match lhs with
        | VarRef { name; _ } -> 
            (try Some (List.assoc name var_values) with Not_found -> try_eval_const lhs)
        | _ -> try_eval_const lhs
      in
      let rhs_val = match rhs with
        | VarRef { name; _ } -> 
            (try Some (List.assoc name var_values) with Not_found -> try_eval_const rhs)
        | _ -> try_eval_const rhs
      in
      (match lhs_val, rhs_val with
      | Some l, Some r ->
          (match op with
          | "LT" | "LTS" -> Some (l < r)
          | "LTE" | "LTES" -> Some (l <= r)
          | "GT" | "GTS" -> Some (l > r)
          | "GTE" | "GTES" -> Some (l >= r)
          | "EQ" -> Some (l = r)
          | "NEQ" -> Some (l <> r)
          | _ -> None)
      | _ -> None)
  | _ -> None

(* ============================================================================
   LOOP UNROLLING
   ============================================================================ *)

(* Substitute a variable with a constant value in an expression *)
let rec substitute_var var_name value expr =
  match expr with
  | VarRef { name; access } when name = var_name ->
      Const { name = string_of_int value; dtype_ref = None }
  
  | BinaryOp { op; lhs; rhs } ->
      BinaryOp {
        op;
        lhs = substitute_var var_name value lhs;
        rhs = substitute_var var_name value rhs;
      }
  
  | UnaryOp { op; operand } ->
      UnaryOp {
        op;
        operand = substitute_var var_name value operand;
      }
  
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
      let substituted_index = substitute_var var_name value index in
      (* Simplify index if it's a Sel with just an expr and lsb=0 *)
      let simplified_index = match substituted_index with
        | Sel { expr = index_expr; lsb = Some (Const { name; _ }); width = None; _ }
          when name = "0" || name = "32'h0" || name = "32'sh0" ->
            index_expr  (* Just use the expr part, drop the [0] bit select *)
        | _ -> substituted_index
      in
      ArraySel {
        expr = substitute_var var_name value expr;
        index = simplified_index;
      }
  
  | Concat { parts } ->
      Concat { parts = List.map (substitute_var var_name value) parts }
  
  | Replicate { src; count; dtype_ref } ->
      Replicate {
        src = substitute_var var_name value src;
        count = substitute_var var_name value count;
        dtype_ref;
      }
  
  | _ -> expr

(* Substitute variable in a statement *)
let rec substitute_var_stmt var_name value stmt =
  match stmt with
  | Assign { lhs; rhs; is_blocking } ->
      Assign {
        lhs = substitute_var var_name value lhs;
        rhs = substitute_var var_name value rhs;
        is_blocking;
      }
  
  | AssignW { lhs; rhs } ->
      AssignW {
        lhs = substitute_var var_name value lhs;
        rhs = substitute_var var_name value rhs;
      }
  
  | If { condition; then_stmt; else_stmt } ->
      If {
        condition = substitute_var var_name value condition;
        then_stmt = substitute_var_stmt var_name value then_stmt;
        else_stmt = Option.map (substitute_var_stmt var_name value) else_stmt;
      }
  
  | Begin { name; stmts; is_generate } ->
      Begin {
        name;
        stmts = List.map (substitute_var_stmt var_name value) stmts;
        is_generate;
      }
  
  | _ -> stmt

(* Extract loop variable name from increment *)
let get_loop_var_from_inc = function
  | Assign { lhs = VarRef { name; _ }; _ } -> Some name
  | _ -> None

(* Enhanced loop unrolling with better pattern matching *)
let unroll_while_loop_enhanced loop_var init_stmt condition body increment =
  if !debug then
    Printf.eprintf "  Analyzing loop for variable: %s\n" loop_var;
  
  (* Extract initial value *)
  let start_val = match init_stmt with
    | Assign { rhs; _ } -> 
        if !debug then Printf.eprintf "  Evaluating init:\n";
        try_eval_const rhs
    | _ -> None
  in
  
  (* Extract increment amount *)
  let inc_amount = match increment with
    | Assign { rhs = BinaryOp { op = "ADD"; lhs; rhs }; _ } ->
        if !debug then Printf.eprintf "  Evaluating increment:\n";
        (match try_eval_const lhs, try_eval_const rhs with
        | Some n, _ when n > 0 -> Some n
        | _, Some n when n > 0 -> Some n
        | _ -> None)
    | _ -> None
  in
  
  (* Extract bound from condition *)
  let (bound_val, comparison_op) = match condition with
    | BinaryOp { op; lhs; rhs } ->
        if !debug then Printf.eprintf "  Evaluating condition (%s):\n" op;
        let left_const = try_eval_const lhs in
        let right_const = try_eval_const rhs in
        (match op, left_const, right_const with
        | "GTS", Some bound, None -> (Some bound, "GTS")  (* bound > i *)
        | "LTS", None, Some bound -> (Some bound, "LTS")  (* i < bound *)
        | "LTES", None, Some bound -> (Some bound, "LTES") (* i <= bound *)
        | "GTES", Some bound, None -> (Some bound, "GTES") (* bound >= i *)
        | _ -> (None, ""))
    | _ -> (None, "")
  in
  
  match start_val, bound_val, inc_amount with
  | Some start, Some bound, Some inc ->
      (* Calculate number of iterations *)
      let num_iters = match comparison_op with
        | "GTS" -> (bound - start) / inc
        | "LTS" -> (bound - start) / inc
        | "LTES" -> ((bound - start) / inc) + 1
        | "GTES" -> ((bound - start) / inc) + 1
        | _ -> 0
      in
      
      if num_iters > 0 && num_iters < max_unroll_iterations then begin
        if !debug then
          Printf.eprintf "  => Unrolling: %d iterations (%d to %d by %d)\n" 
            num_iters start bound inc;
        
        let rec generate_iterations iter_val remaining acc =
          if remaining <= 0 then acc
          else
            let unrolled_body = List.map (substitute_var_stmt loop_var iter_val) body in
            generate_iterations (iter_val + inc) (remaining - 1) (acc @ unrolled_body)
        in
        
        let unrolled = generate_iterations start num_iters [] in
        stats.loops_unrolled <- stats.loops_unrolled + 1;
        Some unrolled
      end else begin
        if !debug then Printf.eprintf "  => Cannot unroll: %d iterations\n" num_iters;
        None
      end
  
  | _ ->
      if !debug then begin
        Printf.eprintf "  => Cannot unroll - missing:\n";
        if start_val = None then Printf.eprintf "     - start value\n";
        if bound_val = None then Printf.eprintf "     - bound value\n";
        if inc_amount = None then Printf.eprintf "     - increment\n";
      end;
      None

(* ============================================================================
   FUNCTION AND TASK INLINING
   ============================================================================ *)

type symbol_table = {
  functions: (string, sv_node) Hashtbl.t;
  tasks: (string, sv_node) Hashtbl.t;
}

let create_symbol_table () = {
  functions = Hashtbl.create 50;
  tasks = Hashtbl.create 50;
}

let rec collect_functions_tasks symtab stmts =
  List.iter (function
    | Func { name; dtype_ref; stmts; vars } as func ->
        Hashtbl.add symtab.functions name func;
        if !debug then Printf.eprintf "Found function: %s\n" name
    | Task { name; dtype_ref; stmts; vars } as task ->
        Hashtbl.add symtab.tasks name task;
        if !debug then Printf.eprintf "Found task: %s\n" name
    | Begin { stmts; _ } -> collect_functions_tasks symtab stmts
    | Always { stmts; _ } -> collect_functions_tasks symtab stmts
    | _ -> ()
  ) stmts

(* Rename variables to avoid conflicts *)
let rename_var suffix var_name = Printf.sprintf "%s_%s" var_name suffix

let rec rename_expr suffix expr =
  match expr with
  | VarRef { name; access } -> VarRef { name = rename_var suffix name; access }
  | BinaryOp { op; lhs; rhs } -> BinaryOp { op; lhs = rename_expr suffix lhs; rhs = rename_expr suffix rhs }
  | UnaryOp { op; operand } -> UnaryOp { op; operand = rename_expr suffix operand }
  | Cond { condition; then_val; else_val } ->
      Cond {
        condition = rename_expr suffix condition;
        then_val = rename_expr suffix then_val;
        else_val = rename_expr suffix else_val;
      }
  | Sel { expr; lsb; width; range } ->
      Sel {
        expr = rename_expr suffix expr;
        lsb = Option.map (rename_expr suffix) lsb;
        width = Option.map (rename_expr suffix) width;
        range;
      }
  | ArraySel { expr; index } -> ArraySel { expr = rename_expr suffix expr; index = rename_expr suffix index }
  | Concat { parts } -> Concat { parts = List.map (rename_expr suffix) parts }
  | _ -> expr

let rec rename_stmt suffix stmt =
  match stmt with
  | Assign { lhs; rhs; is_blocking } ->
      Assign { lhs = rename_expr suffix lhs; rhs = rename_expr suffix rhs; is_blocking }
  | AssignW { lhs; rhs } -> AssignW { lhs = rename_expr suffix lhs; rhs = rename_expr suffix rhs }
  | If { condition; then_stmt; else_stmt } ->
      If {
        condition = rename_expr suffix condition;
        then_stmt = rename_stmt suffix then_stmt;
        else_stmt = Option.map (rename_stmt suffix) else_stmt;
      }
  | Begin { name; stmts; is_generate } ->
      Begin { name; stmts = List.map (rename_stmt suffix) stmts; is_generate }
  | Case { expr; items } ->
      Case {
        expr = rename_expr suffix expr;
        items = List.map (fun { conditions; statements } -> {
          conditions = List.map (rename_expr suffix) conditions;
          statements = List.map (rename_stmt suffix) statements;
        }) items;
      }
  | Var { name; dtype_ref; var_type; direction; value; dtype_name; is_param } ->
      Var { name = rename_var suffix name; dtype_ref; var_type; direction; value; dtype_name; is_param }
  | _ -> stmt

(* Map formal parameters to actual arguments *)
let create_param_map formals actuals =
  let param_map = Hashtbl.create 10 in
  List.iter2 (fun formal actual ->
    match formal with
    | Var { name; _ } -> Hashtbl.add param_map name actual
    | _ -> ()
  ) formals actuals;
  param_map

(* Substitute parameters with arguments *)
let rec substitute_params param_map expr =
  match expr with
  | VarRef { name; access } ->
      (try Hashtbl.find param_map name with Not_found -> expr)
  | BinaryOp { op; lhs; rhs } ->
      BinaryOp { op; lhs = substitute_params param_map lhs; rhs = substitute_params param_map rhs }
  | UnaryOp { op; operand } -> UnaryOp { op; operand = substitute_params param_map operand }
  | Cond { condition; then_val; else_val } ->
      Cond {
        condition = substitute_params param_map condition;
        then_val = substitute_params param_map then_val;
        else_val = substitute_params param_map else_val;
      }
  | Sel { expr; lsb; width; range } ->
      Sel {
        expr = substitute_params param_map expr;
        lsb = Option.map (substitute_params param_map) lsb;
        width = Option.map (substitute_params param_map) width;
        range;
      }
  | ArraySel { expr; index } ->
      ArraySel { expr = substitute_params param_map expr; index = substitute_params param_map index }
  | Concat { parts } -> Concat { parts = List.map (substitute_params param_map) parts }
  | _ -> expr

let rec substitute_params_stmt param_map stmt =
  match stmt with
  | Assign { lhs; rhs; is_blocking } ->
      Assign {
        lhs = substitute_params param_map lhs;
        rhs = substitute_params param_map rhs;
        is_blocking;
      }
  | AssignW { lhs; rhs } ->
      AssignW { lhs = substitute_params param_map lhs; rhs = substitute_params param_map rhs }
  | If { condition; then_stmt; else_stmt } ->
      If {
        condition = substitute_params param_map condition;
        then_stmt = substitute_params_stmt param_map then_stmt;
        else_stmt = Option.map (substitute_params_stmt param_map) else_stmt;
      }
  | Begin { name; stmts; is_generate } ->
      Begin { name; stmts = List.map (substitute_params_stmt param_map) stmts; is_generate }
  | Case { expr; items } ->
      Case {
        expr = substitute_params param_map expr;
        items = List.map (fun { conditions; statements } -> {
          conditions = List.map (substitute_params param_map) conditions;
          statements = List.map (substitute_params_stmt param_map) statements;
        }) items;
      }
  | _ -> stmt

(* Inline a function call *)
let inline_function_call func_name func_def args call_id =
  match func_def with
  | Func { name; dtype_ref; stmts; vars } ->
      if !debug then Printf.eprintf "Inlining function %s (call #%d)\n" func_name call_id;
      let suffix = Printf.sprintf "%s_%d" func_name call_id in
      let param_map = create_param_map vars args in
      let substituted_stmts = List.map (substitute_params_stmt param_map) stmts in
      let renamed_stmts = List.map (rename_stmt suffix) substituted_stmts in
      let return_value = ref None in
      let rec find_return stmt =
        match stmt with
        | Assign { lhs = VarRef { name = var_name; _ }; rhs; _ } when var_name = func_name ->
            return_value := Some rhs
        | Begin { stmts; _ } -> List.iter find_return stmts
        | _ -> ()
      in
      List.iter find_return renamed_stmts;
      stats.functions_inlined <- stats.functions_inlined + 1;
      (renamed_stmts, !return_value)
  | _ -> ([], None)

(* Inline a task call *)
let inline_task_call task_name task_def args call_id =
  match task_def with
  | Task { name; dtype_ref; stmts; vars } ->
      if !debug then Printf.eprintf "Inlining task %s (call #%d)\n" task_name call_id;
      let suffix = Printf.sprintf "%s_%d" task_name call_id in
      let param_map = create_param_map vars args in
      let substituted_stmts = List.map (substitute_params_stmt param_map) stmts in
      let renamed_stmts = List.map (rename_stmt suffix) substituted_stmts in
      stats.tasks_inlined <- stats.tasks_inlined + 1;
      renamed_stmts
  | _ -> []

let call_counter = ref 0

(* ============================================================================
   TRANSFORMATION PASS
   ============================================================================ *)

let rec transform_stmt symtab stmt =
  match stmt with
  | Assign { lhs; rhs; is_blocking } ->
      Assign { lhs; rhs = transform_expr symtab rhs; is_blocking }
  
  | AssignW { lhs; rhs } ->
      AssignW { lhs; rhs = transform_expr symtab rhs }
  
  | TaskRef { name; args } ->
      (try
        let task_def = Hashtbl.find symtab.tasks name in
        incr call_counter;
        let inlined_stmts = inline_task_call name task_def args !call_counter in
        Begin { name = ""; stmts = inlined_stmts; is_generate = false }
      with Not_found ->
        if !debug then Printf.eprintf "Task %s not found for inlining\n" name;
        stmt)
  
  | Begin { name; stmts; is_generate } ->
      let rec process_stmts acc = function
        | [] -> List.rev acc
        
        (* Match for-loop: Var, Assign, While *)
        | (Var { name = var_name; _ } as var_decl) ::
          (Assign { lhs = VarRef { name = init_var; _ }; _ } as init_stmt) ::
          (While { condition; stmts = body; incs = [inc] } as while_stmt) ::
          rest when init_var = var_name ->
            
            if !debug then Printf.eprintf "Found for-loop pattern: %s\n" var_name;
            
            (match inc with
            | Assign { lhs = VarRef { name = inc_var; _ }; _ } when inc_var = var_name ->
                (match unroll_while_loop_enhanced var_name init_stmt condition body inc with
                | Some unrolled ->
                    if !debug then Printf.eprintf "  => Unrolled into %d stmts\n" (List.length unrolled);
                    process_stmts (List.rev_append (List.rev unrolled) acc) rest
                
                | None ->
                    if !debug then Printf.eprintf "  => Could not unroll\n";
                    let t_var = transform_stmt symtab var_decl in
                    let t_init = transform_stmt symtab init_stmt in
                    let t_while = transform_stmt symtab while_stmt in
                    process_stmts (t_while :: t_init :: t_var :: acc) rest)
            
            | _ ->
                let t_var = transform_stmt symtab var_decl in
                let t_init = transform_stmt symtab init_stmt in
                let t_while = transform_stmt symtab while_stmt in
                process_stmts (t_while :: t_init :: t_var :: acc) rest)
        
        | stmt :: rest ->
            process_stmts (transform_stmt symtab stmt :: acc) rest
      in
      
      Begin { name; stmts = process_stmts [] stmts; is_generate }
  
  | While { condition; stmts; incs } ->
      (* Standalone while loop *)
      if !debug then Printf.eprintf "Found standalone While loop\n";
      (match incs with
      | [inc] ->
          (match get_loop_var_from_inc inc with
          | Some var_name ->
              let init_val = Assign { 
                lhs = VarRef { name = var_name; access = "WR" };
                rhs = Const { name = "0"; dtype_ref = None };
                is_blocking = true;
              } in
              (match unroll_while_loop_enhanced var_name init_val condition stmts inc with
              | Some unrolled_stmts -> Begin { name = ""; stmts = unrolled_stmts; is_generate = false }
              | None ->
                  While {
                    condition = transform_expr symtab condition;
                    stmts = List.map (transform_stmt symtab) stmts;
                    incs = List.map (transform_stmt symtab) incs;
                  })
          | None -> stmt)
      | _ -> stmt)
  
  | If { condition; then_stmt; else_stmt } ->
      If {
        condition = transform_expr symtab condition;
        then_stmt = transform_stmt symtab then_stmt;
        else_stmt = Option.map (transform_stmt symtab) else_stmt;
      }
  
  | Always { always; senses; stmts } ->
      Always { always; senses; stmts = List.map (transform_stmt symtab) stmts }
  
  | _ -> stmt

and transform_expr symtab expr =
  match expr with
  | FuncRef { name; args } ->
      (try
        let func_def = Hashtbl.find symtab.functions name in
        incr call_counter;
        let (inlined_body, return_val) = inline_function_call name func_def args !call_counter in
        match return_val with
        | Some ret_expr -> transform_expr symtab ret_expr
        | None -> expr
      with Not_found ->
        if !debug then Printf.eprintf "Function %s not found for inlining\n" name;
        FuncRef { name; args = List.map (transform_expr symtab) args })
  | BinaryOp { op; lhs; rhs } ->
      BinaryOp { op; lhs = transform_expr symtab lhs; rhs = transform_expr symtab rhs }
  | UnaryOp { op; operand } -> UnaryOp { op; operand = transform_expr symtab operand }
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
  | Concat { parts } -> Concat { parts = List.map (transform_expr symtab) parts }
  | Replicate { src; count; dtype_ref } ->
      Replicate {
        src = transform_expr symtab src;
        count = transform_expr symtab count;
        dtype_ref;
      }
  | _ -> expr

let transform_module stmts =
  let symtab = create_symbol_table () in
  collect_functions_tasks symtab stmts;
  if !debug then
    Printf.eprintf "Found %d functions and %d tasks\n"
      (Hashtbl.length symtab.functions)
      (Hashtbl.length symtab.tasks);
  call_counter := 0;
  List.map (transform_stmt symtab) stmts

let rec transform_ast node =
  match node with
  | Netlist modules -> Netlist (List.map transform_ast modules)
  | Module { name; stmts } ->
      if !debug then Printf.eprintf "Transforming module: %s\n" name;
      Module { name; stmts = transform_module stmts }
  | _ -> node

let transform ?(verbose=false) ast =
  debug := verbose;
  stats.loops_unrolled <- 0;
  stats.functions_inlined <- 0;
  stats.tasks_inlined <- 0;
  
  let transformed = transform_ast ast in
  
  if verbose then begin
    Printf.printf "Transformation Statistics:\n";
    Printf.printf "  Loops unrolled: %d\n" stats.loops_unrolled;
    Printf.printf "  Functions inlined: %d\n" stats.functions_inlined;
    Printf.printf "  Tasks inlined: %d\n" stats.tasks_inlined;
  end;
  
  transformed
