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
   SSA (Single Static Assignment) CONVERSION
   ============================================================================ *)

(* Track variable versions for SSA conversion *)
type ssa_context = {
  versions: (string, int) Hashtbl.t;
  mutable temp_counter: int;
}

let create_ssa_context () = {
  versions = Hashtbl.create 50;
  temp_counter = 0;
}

(* Get current version of a variable *)
let get_version ctx var_name =
  try Hashtbl.find ctx.versions var_name
  with Not_found -> 0

(* Increment version and return new versioned name *)
let new_version ctx var_name =
  let current = get_version ctx var_name in
  let next = current + 1 in
  Hashtbl.replace ctx.versions var_name next;
  Printf.sprintf "%s_%d" var_name next

(* Get versioned name for reading *)
let get_versioned_name ctx var_name =
  let version = get_version ctx var_name in
  if version = 0 then var_name
  else Printf.sprintf "%s_%d" var_name version

(* Convert expression to SSA form *)
let rec ssa_expr ctx expr =
  match expr with
  | VarRef { name; access = "RD" } ->
      let versioned = get_versioned_name ctx name in
      VarRef { name = versioned; access = "RD" }
  | VarRef _ as v -> v
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
      ArraySel {
        expr = ssa_expr ctx expr;
        index = ssa_expr ctx index;
      }
  | Concat { parts } ->
      Concat { parts = List.map (ssa_expr ctx) parts }
  | Replicate { src; count; dtype_ref } ->
      Replicate {
        src = ssa_expr ctx src;
        count = ssa_expr ctx count;
        dtype_ref;
      }
  | _ -> expr

(* Flatten nested Begin blocks *)
let rec flatten_begins stmts =
  List.concat_map (function
    | Begin { stmts = inner; is_generate = false; _ } ->
        flatten_begins inner
    | stmt -> [stmt]
  ) stmts

(* Detect multi-assigned variables *)
let rec find_multi_assigned stmts =
  let assignments = Hashtbl.create 20 in
  let flat_stmts = flatten_begins stmts in
  
  let rec count_assigns stmt =
    match stmt with
    | Assign { lhs = VarRef { name; _ }; _ } ->
        let count = try Hashtbl.find assignments name with Not_found -> 0 in
        Hashtbl.replace assignments name (count + 1)
    | Begin { stmts; _ } ->
        List.iter count_assigns (flatten_begins stmts)
    | If { then_stmt; else_stmt; _ } ->
        count_assigns then_stmt;
        Option.iter count_assigns else_stmt
    | _ -> ()
  in
  
  List.iter count_assigns flat_stmts;
  
  let multi = ref [] in
  Hashtbl.iter (fun name count ->
    if count > 1 then multi := name :: !multi
  ) assignments;
  !multi

(* Convert statement to SSA form *)
let rec ssa_stmt ctx multi_assigned stmt =
  match stmt with
  | Assign { lhs = VarRef { name; access }; rhs; is_blocking } ->
      let ssa_rhs = ssa_expr ctx rhs in
      if List.mem name multi_assigned then begin
        let new_name = new_version ctx name in
        if !debug then
          Printf.eprintf "    SSA: %s -> %s\n" name new_name;
        Assign {
          lhs = VarRef { name = new_name; access };
          rhs = ssa_rhs;
          is_blocking;
        }
      end else begin
        Assign { lhs = VarRef { name; access }; rhs = ssa_rhs; is_blocking }
      end
  | Assign { lhs; rhs; is_blocking } ->
      Assign {
        lhs = ssa_expr ctx lhs;
        rhs = ssa_expr ctx rhs;
        is_blocking;
      }
  | AssignW { lhs; rhs } ->
      AssignW {
        lhs = ssa_expr ctx lhs;
        rhs = ssa_expr ctx rhs;
      }
  | Begin { name; stmts; is_generate } ->
      let ssa_stmts = List.map (ssa_stmt ctx multi_assigned) stmts in
      Begin { name; stmts = ssa_stmts; is_generate }
  | If { condition; then_stmt; else_stmt } ->
      If {
        condition = ssa_expr ctx condition;
        then_stmt = ssa_stmt ctx multi_assigned then_stmt;
        else_stmt = Option.map (ssa_stmt ctx multi_assigned) else_stmt;
      }
  | _ -> stmt

(* Add final assignments *)
let add_final_assignments ctx multi_assigned stmts =
  let finals = List.filter_map (fun var_name ->
    let version = get_version ctx var_name in
    if version > 0 then
      Some (Assign {
        lhs = VarRef { name = var_name; access = "WR" };
        rhs = VarRef { name = Printf.sprintf "%s_%d" var_name version; access = "RD" };
        is_blocking = true;
      })
    else
      None
  ) multi_assigned in
  stmts @ finals

(* Collect SSA variable declarations that need to be added to module *)
let ssa_var_decls = ref []

let clear_ssa_var_decls () =
  ssa_var_decls := []

let add_ssa_var_decl var_decl =
  ssa_var_decls := var_decl :: !ssa_var_decls

let get_ssa_var_decls () =
  List.rev !ssa_var_decls

(* Add variable declarations for SSA versioned names *)
let collect_ssa_declarations ctx multi_assigned original_vars =
  List.iter (fun var_name ->
    let version = get_version ctx var_name in
    if version > 0 then begin
      (* Find original variable to get its type *)
      let original_var = List.find_opt (function
        | Var { name = n; _ } when n = var_name -> true
        | _ -> false
      ) original_vars in
      
      match original_var with
      | Some (Var { dtype_ref; dtype_name; _ }) ->
          (* Create declarations for cnt_1, cnt_2, ..., cnt_N *)
          for i = 1 to version do
            let versioned_name = Printf.sprintf "%s_%d" var_name i in
            let var_decl = Var {
              name = versioned_name;
              dtype_ref;
              var_type = "VAR";
              direction = "NONE";
              value = None;
              dtype_name;
              is_param = false;
            } in
            if !debug then
              Printf.eprintf "    Creating var decl: %s\n" versioned_name;
            add_ssa_var_decl var_decl
          done
      | _ -> ()
    end
  ) multi_assigned

(* Convert to SSA form *)
let convert_to_ssa_with_decls original_vars stmts =
  if !debug then
    Printf.eprintf "  Converting to SSA form\n";
  
  let flat_stmts = flatten_begins stmts in
  let multi_assigned = find_multi_assigned flat_stmts in
  
  if List.length multi_assigned = 0 then begin
    if !debug then
      Printf.eprintf "    No multi-assigned variables\n";
    stmts
  end else begin
    if !debug then begin
      Printf.eprintf "    Multi-assigned variables: %s\n" 
        (String.concat ", " multi_assigned);
    end;
    
    let ctx = create_ssa_context () in
    let ssa_stmts = List.map (ssa_stmt ctx multi_assigned) flat_stmts in
    let final_stmts = add_final_assignments ctx multi_assigned ssa_stmts in
    
    (* Collect variable declarations for later insertion at module level *)
    collect_ssa_declarations ctx multi_assigned original_vars;
    
    if !debug then
      Printf.eprintf "    SSA conversion complete: %d statements -> %d statements\n"
        (List.length flat_stmts) (List.length final_stmts);
    
    final_stmts
  end

(* Original convert_to_ssa without declarations (for when we don't have original_vars) *)
let convert_to_ssa stmts =
  convert_to_ssa_with_decls [] stmts

(* ============================================================================
   CONSTANT EXPRESSION EVALUATION
   ============================================================================ *)

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
      (try
        if String.contains name '\'' then
          let parts = String.split_on_char '\'' name in
          match parts with
          | width :: rest ->
              let value_str = String.concat "'" rest in
              if !debug then
                Printf.eprintf "      width=%s, value=%s\n" width value_str;
              
              if String.length value_str >= 2 then
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

(* ============================================================================
   LOOP UNROLLING
   ============================================================================ *)

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
      let simplified_index = match substituted_index with
        | Sel { expr = index_expr; lsb = Some (Const { name; _ }); width = None; _ }
          when name = "0" || name = "32'h0" || name = "32'sh0" ->
            index_expr
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

let get_loop_var_from_inc = function
  | Assign { lhs = VarRef { name; _ }; _ } -> Some name
  | _ -> None

let unroll_while_loop_enhanced loop_var init_stmt condition body increment =
  if !debug then
    Printf.eprintf "  Analyzing loop for variable: %s\n" loop_var;
  
  let start_val = match init_stmt with
    | Assign { rhs; _ } -> 
        if !debug then Printf.eprintf "  Evaluating init:\n";
        try_eval_const rhs
    | _ -> None
  in
  
  let inc_amount = match increment with
    | Assign { rhs = BinaryOp { op = "ADD"; lhs; rhs }; _ } ->
        if !debug then Printf.eprintf "  Evaluating increment:\n";
        (match try_eval_const lhs, try_eval_const rhs with
        | Some n, _ when n > 0 -> Some n
        | _, Some n when n > 0 -> Some n
        | _ -> None)
    | _ -> None
  in
  
  let (bound_val, comparison_op) = match condition with
    | BinaryOp { op; lhs; rhs } ->
        if !debug then Printf.eprintf "  Evaluating condition (%s):\n" op;
        let left_const = try_eval_const lhs in
        let right_const = try_eval_const rhs in
        (match op, left_const, right_const with
        | "GTS", Some bound, None -> (Some bound, "GTS")
        | "LTS", None, Some bound -> (Some bound, "LTS")
        | "LTES", None, Some bound -> (Some bound, "LTES")
        | "GTES", Some bound, None -> (Some bound, "GTES")
        | _ -> (None, ""))
    | _ -> (None, "")
  in
  
  match start_val, bound_val, inc_amount with
  | Some start, Some bound, Some inc ->
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
   FUNCTION AND TASK INLINING (stubs)
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
    | Func _ as func -> ()
    | Task _ as task -> ()
    | Begin { stmts; _ } | Always { stmts; _ } -> collect_functions_tasks symtab stmts
    | _ -> ()
  ) stmts

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
  
  | Always { always; senses; stmts } ->
      if !debug then
        Printf.eprintf "  Processing Always block: %s\n" always;
      
      let transformed = List.map (transform_stmt symtab) stmts in
      let ssa_stmts = convert_to_ssa transformed in
      
      Always { always; senses; stmts = ssa_stmts }
  
  | Begin { name; stmts; is_generate } ->
      let rec process_stmts acc = function
        | [] -> List.rev acc
        | (Var { name = var_name; _ } as var_decl) ::
          (Assign { lhs = VarRef { name = init_var; _ }; _ } as init_stmt) ::
          (While { condition; stmts = body; incs = [inc] } as while_stmt) ::
          rest when init_var = var_name ->
            
            if !debug then
              Printf.eprintf "Found for-loop pattern: %s\n" var_name;
            
            (match inc with
            | Assign { lhs = VarRef { name = inc_var; _ }; _ } when inc_var = var_name ->
                (match unroll_while_loop_enhanced var_name init_stmt condition body inc with
                | Some unrolled ->
                    if !debug then
                      Printf.eprintf "  => Unrolled into %d stmts\n" (List.length unrolled);
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
      
      let transformed_stmts = process_stmts [] stmts in
      
      if is_generate then begin
        if !debug then Printf.eprintf "  Processing generate block\n";
        let ssa_stmts = convert_to_ssa transformed_stmts in
        Begin { name; stmts = ssa_stmts; is_generate }
      end else begin
        Begin { name; stmts = transformed_stmts; is_generate }
      end
  
  | If { condition; then_stmt; else_stmt } ->
      If {
        condition = transform_expr symtab condition;
        then_stmt = transform_stmt symtab then_stmt;
        else_stmt = Option.map (transform_stmt symtab) else_stmt;
      }
  
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
