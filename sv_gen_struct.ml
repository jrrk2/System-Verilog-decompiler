open Sv_ast

let debug = ref false
let warnings = ref []
let inst_counter = ref 0

(* Context for tracking variables and their properties *)
type var_info = {
  name: string;
  dtype_ref: sv_type option;
  dtype_name: string;
  is_reg: bool;
  width: int;
}

type structural_context = {
  variables: (string, var_info) Hashtbl.t;
  wires: string list ref;
  regs: string list ref;
  instances: string list ref;
  mutable in_sequential: bool;
  mutable current_clock: string option;
  mutable current_reset: string option;
}

(* Enhanced always block classification *)
type always_block_type =
  | Sequential of {
      clock: string;
      clock_edge: [`Posedge | `Negedge];
      reset: (string * [`Async | `Sync]) option;
    }
  | Combinational
  | Latch
  | Unsynthesizable of string

(* Detect edge type from sensitivity *)
let detect_edge edge_str =
  let lower = String.lowercase_ascii edge_str in
  if String.contains lower 'p' || lower = "posedge" then `Posedge
  else if String.contains lower 'n' || lower = "negedge" then `Negedge
  else failwith ("Unknown edge type: " ^ edge_str)

(* Detect clock signals *)
let is_clock_signal name =
  let lower = String.lowercase_ascii name in
  lower = "clk" || lower = "clock" || String.contains lower 'c' && String.contains lower 'l' && String.contains lower 'k'

(* Detect reset signals *)
let is_reset_signal name =
  let lower = String.lowercase_ascii name in
  lower = "rst" || lower = "reset" || lower = "rstn" || lower = "rst_n"

(* Enhanced sensitivity analysis *)
let analyze_sensitivity_detailed senses =
  let clocks = ref [] in
  let resets = ref [] in
  let other_signals = ref [] in
  
  let rec process_sense = function
    | SenTree items ->
        List.iter process_sense items
    | SenItem { edge_str; signal } ->
        (match signal with
        | VarRef { name; _ } ->
            let edge = detect_edge edge_str in
            if is_clock_signal name then
              clocks := (name, edge) :: !clocks
            else if is_reset_signal name then
              resets := (name, edge) :: !resets
            else
              other_signals := (name, edge) :: !other_signals
        | _ -> ())
    | _ -> ()
  in
  
  List.iter process_sense senses;
  (!clocks, !resets, !other_signals)

(* Detect reset pattern from statements *)
let rec detect_reset_pattern stmt =
  let rec has_only_resets = function
    | Assign { rhs = Const { name; _ }; _ } when name = "0" || name = "'0" || name = "1'b0" -> true
    | Begin { stmts; _ } -> List.for_all has_only_resets stmts
    | _ -> false
  in
  
  match stmt with
  | If { condition; then_stmt; _ } ->
      (match condition with
      | VarRef { name; _ } when is_reset_signal name ->
          if has_only_resets then_stmt then Some (name, `Sync) else None
      | UnaryOp { op = "LOGNOT"; operand = VarRef { name; _ } } when is_reset_signal name ->
          if has_only_resets then_stmt then Some (name, `Sync) else None
      | _ -> None)
  | Begin { stmts; _ } ->
      (match stmts with first :: _ -> detect_reset_pattern first | [] -> None)
  | _ -> None

(* Classify always block type - THIS IS THE KEY FUNCTION *)
let classify_always_block always_type senses stmts =
  match always_type with
  | "always_comb" -> Combinational
  | "always_latch" -> Latch
  | "always_ff" ->
      let (clocks, resets, _) = analyze_sensitivity_detailed senses in
      (match clocks, resets with
      | (clk, clk_edge) :: [], [] ->
          Sequential { clock = clk; clock_edge = clk_edge; reset = None }
      | (clk, clk_edge) :: [], (rst, _) :: [] ->
          Sequential { clock = clk; clock_edge = clk_edge; reset = Some (rst, `Async) }
      | _ -> Unsynthesizable "always_ff must have exactly one clock")
  | "always" ->
      let (clocks, resets, others) = analyze_sensitivity_detailed senses in
      if List.length clocks > 0 then
        (match clocks, resets with
        | (clk, clk_edge) :: [], [] ->
            (* Check for sync reset in body *)
            let sync_reset = detect_reset_pattern (Begin { name = ""; stmts; is_generate = false }) in
            Sequential { clock = clk; clock_edge = clk_edge; reset = sync_reset }
        | (clk, clk_edge) :: [], (rst, _) :: [] ->
            Sequential { clock = clk; clock_edge = clk_edge; reset = Some (rst, `Async) }
        | _ :: _ :: _, _ -> Unsynthesizable "Multiple clocks not supported"
        | _, _ :: _ :: _ -> Unsynthesizable "Multiple resets not supported"
        | _ -> Unsynthesizable "Invalid clock configuration")
      else if List.length others > 0 || List.length senses = 0 then
        Combinational
      else
        Unsynthesizable "Invalid sensitivity list"
  | _ -> Unsynthesizable ("Unknown always type: " ^ always_type)

(* Generate unique instance names *)
let gen_inst_name prefix =
  incr inst_counter;
  Printf.sprintf "%s_%d" prefix !inst_counter

(* Add a warning message *)
let add_warning msg =
  warnings := msg :: !warnings;
  if !debug then Printf.eprintf "WARNING: %s\n" msg

(* Get all warnings *)
let get_warnings () = List.rev !warnings

(* Clear warnings *)
let clear_warnings () = 
  warnings := [];
  inst_counter := 0

(* Type resolution helpers *)
let resolve_type = function
  | Some (RefType { name; _ }) -> name
  | Some (BasicType { keyword; range = Some r; _ }) -> keyword ^ " [" ^ r ^ "]"
  | Some (BasicType { keyword; range = None; _ }) -> keyword
  | Some (EnumType { name; _ }) -> name
  | _ -> "logic"

let resolve_type_with_brackets dtype_name dtype_ref =
  match dtype_ref with
  | Some (ArrayType _) ->
      if dtype_name <> "" && dtype_name <> "logic" then 
        dtype_name 
      else 
        (match dtype_ref with
        | Some (ArrayType { base; _ }) -> resolve_type (Some base)
        | _ -> "logic")
  | _ ->
      let base_type = if dtype_name <> "" && dtype_name <> "logic" then dtype_name else resolve_type dtype_ref in
      let fixed_type = Str.global_replace (Str.regexp "\\[\\[\\([^]]+\\)\\]\\]") "[\\1]" base_type in
      if String.contains fixed_type ' ' && not (String.contains fixed_type '[') then
        (match String.split_on_char ' ' fixed_type with
        | [keyword; range] when String.contains range ':' -> 
            Printf.sprintf "%s [%s]" keyword range
        | _ -> fixed_type)
      else fixed_type

(* Extract bit width from type or expression *)
let get_bit_width dtype_ref dtype_name =
  match dtype_ref with
  | Some (BasicType { range = Some r; _ }) ->
      (try
        let parts = String.split_on_char ':' r in
        match parts with
        | [msb; lsb] -> 
            let m = int_of_string (String.trim msb) in
            let l = int_of_string (String.trim lsb) in
            abs (m - l) + 1
        | _ -> 1
      with _ -> 1)
  | Some (ArrayType { range; _ }) ->
      (try
        let parts = String.split_on_char ':' range in
        match parts with
        | [msb; lsb] -> 
            let m = int_of_string (String.trim msb) in
            let l = int_of_string (String.trim lsb) in
            abs (m - l) + 1
        | _ -> 1
      with _ -> 1)
  | _ -> 1

(* Add variable to context *)
let add_var ctx name dtype_ref dtype_name is_reg =
  let width = get_bit_width dtype_ref dtype_name in
  Hashtbl.add ctx.variables name { name; dtype_ref; dtype_name; is_reg; width }

(* Get variable info *)
let get_var ctx name =
  try Some (Hashtbl.find ctx.variables name)
  with Not_found -> None

(* Infer width from expression *)
let rec infer_width ctx = function
  | VarRef { name; _ } ->
      (match get_var ctx name with
      | Some { width; _ } -> width
      | None -> 1)
  | Const { name; _ } ->
      (* Try to parse constant width *)
      if String.contains name '\'' then
        (try
          let parts = String.split_on_char '\'' name in
          match parts with
          | width_str :: _ -> int_of_string (String.trim width_str)
          | _ -> 32
        with _ -> 32)
      else 32
  | BinaryOp { lhs; rhs; _ } ->
      max (infer_width ctx lhs) (infer_width ctx rhs)
  | UnaryOp { operand; _ } ->
      infer_width ctx operand
  | Sel { expr; _ } | ArraySel { expr; _ } ->
      infer_width ctx expr
  | Cond { then_val; else_val; _ } ->
      max (infer_width ctx then_val) (infer_width ctx else_val)
  | Concat { parts } ->
      List.fold_left (fun acc p -> acc + infer_width ctx p) 0 parts
  | _ -> 1

(* Check if an expression depends on a variable (for loop detection) *)
let rec expr_depends_on ctx var_name = function
  | VarRef { name; _ } -> name = var_name
  | BinaryOp { lhs; rhs; _ } -> 
      expr_depends_on ctx var_name lhs || expr_depends_on ctx var_name rhs
  | UnaryOp { operand; _ } -> 
      expr_depends_on ctx var_name operand
  | Cond { condition; then_val; else_val } ->
      expr_depends_on ctx var_name condition ||
      expr_depends_on ctx var_name then_val ||
      expr_depends_on ctx var_name else_val
  | Sel { expr; _ } | ArraySel { expr; _ } ->
      expr_depends_on ctx var_name expr
  | Concat { parts } ->
      List.exists (expr_depends_on ctx var_name) parts
  | Replicate { src; _ } ->
      expr_depends_on ctx var_name src
  | _ -> false

let create_context () = {
  variables = Hashtbl.create 100;
  wires = ref [];
  regs = ref [];
  instances = ref [];
  in_sequential = false;
  current_clock = None;
  current_reset = None;
}

(* Infer width from expression *)
let rec infer_width ctx = function
  | VarRef { name; _ } ->
      (match get_var ctx name with
      | Some { width; _ } -> width
      | None -> 1)
  | Const { name; _ } ->
      (* Try to parse constant width *)
      if String.contains name '\'' then
        (try
          let parts = String.split_on_char '\'' name in
          match parts with
          | width_str :: _ -> int_of_string (String.trim width_str)
          | _ -> 32
        with _ -> 32)
      else 32
  | BinaryOp { lhs; rhs; _ } ->
      max (infer_width ctx lhs) (infer_width ctx rhs)
  | UnaryOp { operand; _ } ->
      infer_width ctx operand
  | Sel { expr; _ } | ArraySel { expr; _ } ->
      infer_width ctx expr
  | Cond { then_val; else_val; _ } ->
      max (infer_width ctx then_val) (infer_width ctx else_val)
  | Concat { parts } ->
      List.fold_left (fun acc p -> acc + infer_width ctx p) 0 parts
  | _ -> 1

(* Analyze sensitivity list to determine block type *)
let analyze_sensitivity senses =
  let has_posedge_clk = ref false in
  let has_negedge_clk = ref false in
  let clock_name = ref None in
  let reset_name = ref None in
  
  (* Recursive helper to handle nested SenTree *)
  let rec process_sense = function
    | SenTree items ->
        List.iter process_sense items
    | SenItem { edge_str; signal } ->
        (match signal with
        | VarRef { name; _ } ->
            if String.contains edge_str 'p' || edge_str = "posedge" then begin
              if is_clock_signal name then begin
                has_posedge_clk := true;
                clock_name := Some name
              end else if is_reset_signal name then
                reset_name := Some name
            end else if String.contains edge_str 'n' || edge_str = "negedge" then begin
              if is_clock_signal name then begin
                has_negedge_clk := true;
                clock_name := Some name
              end else if is_reset_signal name then
                reset_name := Some name
            end
        | _ -> ())
    | _ -> ()
  in
  
  List.iter process_sense senses;
  
  (!has_posedge_clk || !has_negedge_clk, !clock_name, !reset_name)

(* Generate structural primitive for binary operation *)
let gen_binary_op ctx op lhs rhs result_wire width =
  let inst_name = gen_inst_name "op" in
  let module_name = match op with
    | "ADD" -> "adder"
    | "SUB" -> "subtractor"
    | "MUL" | "MULS" -> "multiplier"
    | "DIV" | "DIVS" -> "divider"
    | "AND" -> "bitwise_and"
    | "OR" -> "bitwise_or"
    | "XOR" -> "bitwise_xor"
    | "EQ" -> "comparator_eq"
    | "NEQ" -> "comparator_neq"
    | "LT" | "LTS" -> "comparator_lt"
    | "GT" | "GTS" -> "comparator_gt"
    | "LTE" | "LTES" -> "comparator_lte"
    | "GTE" | "GTES" -> "comparator_gte"
    | "SHIFTL" -> "shifter_left"
    | "SHIFTR" -> "shifter_right"
    | "SHIFTRS" -> "shifter_right_arith"
    | _ -> "binary_op"
  in
  ctx.instances := 
    (Printf.sprintf "  %s #(.WIDTH(%d)) %s (.a(%s), .b(%s), .out(%s));" 
      module_name width inst_name lhs rhs result_wire) :: !(ctx.instances);
  result_wire

(* Generate structural primitive for unary operation *)
let gen_unary_op ctx op operand result_wire width =
  let inst_name = gen_inst_name "op" in
  let module_name = match op with
    | "NOT" -> "bitwise_not"
    | "LOGNOT" -> "logical_not"
    | "REDAND" -> "reduction_and"
    | "REDOR" -> "reduction_or"
    | "REDXOR" -> "reduction_xor"
    | "NEGATE" -> "negator"
    | _ -> "unary_op"
  in
  ctx.instances := 
    (Printf.sprintf "  %s #(.WIDTH(%d)) %s (.in(%s), .out(%s));" 
      module_name width inst_name operand result_wire) :: !(ctx.instances);
  result_wire

(* Generate structural multiplexer *)
let gen_mux ctx sel in0 in1 result_wire width =
  let inst_name = gen_inst_name "mux" in
  ctx.instances := 
    (Printf.sprintf "  mux2 #(.WIDTH(%d)) %s (.sel(%s), .in0(%s), .in1(%s), .out(%s));" 
      width inst_name sel in0 in1 result_wire) :: !(ctx.instances);
  result_wire

let gen_dff_en ctx clk rst en d q width reset_val =
  let inst_name = gen_inst_name "dff" in
  
  (* Always include all ports, tie off if not used *)
  let rst_port = match rst with
    | Some r -> Printf.sprintf ".rst(%s)" r
    | None -> ".rst(1'b0)"  (* Tie to 0 if no reset *)
  in
  
  let en_port = match en with
    | Some e -> Printf.sprintf ".en(%s)" e
    | None -> ".en(1'b1)"   (* Always enabled if no enable *)
  in
  
  ctx.instances := 
    (Printf.sprintf "  dff_en #(.WIDTH(%d), .RESET_VAL(%d)) %s (.clk(%s), %s, %s, .d(%s), .q(%s));" 
      width reset_val inst_name clk rst_port en_port d q) :: !(ctx.instances);
  q


(* Generate latch primitive *)
let gen_latch_en ctx en d q width =
  let inst_name = gen_inst_name "latch" in
  
  let en_port = match en with
    | Some e -> Printf.sprintf ".en(%s)" e
    | None -> ".en(1'b1)"
  in
  
  ctx.instances := 
    (Printf.sprintf "  latch_en #(.WIDTH(%d)) %s (%s, .d(%s), .q(%s));" 
      width inst_name en_port d q) :: !(ctx.instances);
  q

(* Enhanced latch detection and handling *)
let detect_latch_enable stmt =
  match stmt with
  | If { condition = VarRef { name; _ }; _ } -> Some name
  | If { condition = UnaryOp { op = "LOGNOT"; operand = VarRef { name; _ } }; _ } -> 
      Some name  (* Active low enable *)
  | _ -> None
  
(* Generate memory instance *)
let gen_memory ctx addr_width data_width depth we clk addr din dout name =
  let inst_name = gen_inst_name "mem" in
  ctx.instances := 
    (Printf.sprintf "  memory #(.ADDR_WIDTH(%d), .DATA_WIDTH(%d), .DEPTH(%d)) %s (.clk(%s), .we(%s), .addr(%s), .din(%s), .dout(%s));" 
      addr_width data_width depth inst_name clk we addr din dout) :: !(ctx.instances);
  dout
  
(* Convert expression to structural form *)
let rec structural_expr ctx expr =
  match expr with
  | VarRef { name; _ } -> name
  
  | Const { name; _ } -> name
  
  | BinaryOp { op; lhs; rhs } ->
      let lhs_wire = structural_expr ctx lhs in
      let rhs_wire = structural_expr ctx rhs in
      let result_wire = gen_inst_name "wire" in
      let width = max (infer_width ctx lhs) (infer_width ctx rhs) in
      ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (width-1) result_wire) :: !(ctx.wires);
      gen_binary_op ctx op lhs_wire rhs_wire result_wire width
  
  | UnaryOp { op; operand } ->
      let operand_wire = structural_expr ctx operand in
      let result_wire = gen_inst_name "wire" in
      let width = infer_width ctx operand in
      ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (width-1) result_wire) :: !(ctx.wires);
      gen_unary_op ctx op operand_wire result_wire width
  
  | Cond { condition; then_val; else_val } ->
      let sel = structural_expr ctx condition in
      let in1 = structural_expr ctx then_val in
      let in0 = structural_expr ctx else_val in
      let result_wire = gen_inst_name "wire" in
      let width = max (infer_width ctx then_val) (infer_width ctx else_val) in
      ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (width-1) result_wire) :: !(ctx.wires);
      gen_mux ctx sel in0 in1 result_wire width
  
  | Sel { expr; lsb; width; _ } ->
      let base = structural_expr ctx expr in
      (match lsb, width with
      | Some l, Some w ->
          let lsb_str = structural_expr ctx l in
          let width_str = structural_expr ctx w in
          Printf.sprintf "%s[%s +: %s]" base lsb_str width_str
      | Some l, None ->
          let lsb_str = structural_expr ctx l in
          Printf.sprintf "%s[%s]" base lsb_str
      | _ -> base)
  
  | ArraySel { expr; index } ->
      let base = structural_expr ctx expr in
      let idx = structural_expr ctx index in
      Printf.sprintf "%s[%s]" base idx
  
  | Concat { parts } ->
      let part_strs = List.map (structural_expr ctx) parts in
      Printf.sprintf "{%s}" (String.concat ", " part_strs)
  
  | Replicate { src; count; _ } ->
      let src_str = structural_expr ctx src in
      let count_str = structural_expr ctx count in
      Printf.sprintf "{%s{%s}}" count_str src_str
  
  | FuncRef { name = "clog2"; args } ->
      (* Special handling for $clog2 - evaluate at elaboration *)
      let arg_str = match args with
        | arg :: _ -> structural_expr ctx arg
        | [] -> "1"
      in
      Printf.sprintf "$clog2(%s)" arg_str
  
  | _ ->
      add_warning "Unsupported expression in structural conversion";
      "/* unsupported */"

(* Convert assignment to structural form *)
and structural_assign ctx lhs rhs is_sequential =
  let lhs_name = match lhs with
    | VarRef { name; _ } -> name
    | Sel _ | ArraySel _ -> structural_expr ctx lhs
    | _ -> "unknown"
  in
  let rhs_wire = structural_expr ctx rhs in
  
  if is_sequential then begin
    (* Sequential assignment - need flip-flop *)
    let width = match get_var ctx lhs_name with
      | Some { width; _ } -> width
      | None -> infer_width ctx rhs
    in
    let d_wire = rhs_wire in
    let q_wire = lhs_name in
    match ctx.current_clock with
    | Some clk ->
        let _ = gen_dff_en ctx clk ctx.current_reset None d_wire q_wire width 0 in
        ()
    | None ->
        add_warning "Sequential assignment without clock context"
  end else begin
    (* Combinational assignment - continuous assign *)
    ctx.instances := 
      (Printf.sprintf "  assign %s = %s;" lhs_name rhs_wire) :: !(ctx.instances)
  end

(* Convert if statement to structural muxes *)
and structural_if ctx condition then_stmt else_stmt =
  (* Special case: if inside sequential block might be enable or reset *)
  if ctx.in_sequential then begin
    (* Check if this is an enable condition *)
    let is_enable_condition = match condition with
      | VarRef { name; _ } when not (is_reset_signal name) -> true
      | _ -> false
    in
    
    if is_enable_condition then begin
      (* This is an enable condition - extract the enable signal *)
      let enable_sig = match condition with
        | VarRef { name; _ } -> Some name
        | _ -> None
      in
      
      (* Process the enabled assignment *)
      match then_stmt with
      | Assign { lhs; rhs; _ } ->
          let lhs_name = match lhs with
            | VarRef { name; _ } -> name
            | _ -> structural_expr ctx lhs
          in
          let rhs_wire = structural_expr ctx rhs in
          let width = match get_var ctx lhs_name with
            | Some { width; _ } -> width
            | None -> infer_width ctx rhs
          in
          
          (* Generate DFF with enable *)
          (match ctx.current_clock, enable_sig with
          | Some clk, Some en ->
              let _ = gen_dff_en ctx clk ctx.current_reset (Some en) rhs_wire lhs_name width 0 in
              ()
          | Some clk, None ->
              let _ = gen_dff_en ctx clk ctx.current_reset None rhs_wire lhs_name width 0 in
              ()
          | None, _ ->
              add_warning "Enable condition in sequential block without clock")
          
      | Begin { stmts; _ } ->
          (* Multiple statements under enable *)
          List.iter (function
            | Assign { lhs; rhs; _ } ->
                let lhs_name = match lhs with
                  | VarRef { name; _ } -> name
                  | _ -> structural_expr ctx lhs
                in
                let rhs_wire = structural_expr ctx rhs in
                let width = match get_var ctx lhs_name with
                  | Some { width; _ } -> width
                  | None -> infer_width ctx rhs
                in
                (match ctx.current_clock, enable_sig with
                | Some clk, Some en ->
                    let _ = gen_dff_en ctx clk ctx.current_reset (Some en) rhs_wire lhs_name width 0 in
                    ()
                | _ -> ())
            | _ -> ()
          ) stmts
          
      | _ -> ()
    end else begin
      (* This might be a reset check - handle as mux for now *)
      (* TODO: Better reset pattern detection *)
      structural_if_as_mux ctx condition then_stmt else_stmt
    end
  end else begin
    (* Combinational if - always use mux *)
    structural_if_as_mux ctx condition then_stmt else_stmt
  end

(* Helper: Convert if to mux tree (original implementation) *)
and structural_if_as_mux ctx condition then_stmt else_stmt =
  let cond_wire = structural_expr ctx condition in
  
  (* Extract assignments from branches *)
  let extract_assigns stmt =
    match stmt with
    | Assign { lhs; rhs; _ } -> [(lhs, rhs)]
    | Begin { stmts; _ } ->
        List.filter_map (function
          | Assign { lhs; rhs; _ } -> Some (lhs, rhs)
          | _ -> None
        ) stmts
    | _ -> []
  in
  
  let then_assigns = extract_assigns then_stmt in
  let else_assigns = match else_stmt with
    | Some stmt -> extract_assigns stmt
    | None -> []
  in
  
  (* For each variable assigned, generate a mux *)
  let process_var (lhs, then_rhs) =
    let lhs_name = match lhs with
      | VarRef { name; _ } -> name
      | _ -> structural_expr ctx lhs
    in
    
    (* Find corresponding else assignment or use current value *)
    let else_rhs = 
      try
        let (_, rhs) = List.find (fun (l, _) ->
          match l with
          | VarRef { name; _ } -> name = lhs_name
          | _ -> false
        ) else_assigns in
        rhs
      with Not_found ->
        (* NO ELSE BRANCH - different handling for sequential vs combinational *)
        if ctx.in_sequential then
          VarRef { name = lhs_name; access = "RD" }  (* OK in sequential - keeps value *)
        else begin
          (* In combinational, use default value to avoid loop *)
          add_warning (Printf.sprintf "Incomplete if for %s - using default 0" lhs_name);
          Const { name = "'0"; dtype_ref = None }
        end
    in
    
    let then_wire = structural_expr ctx then_rhs in
    let else_wire = structural_expr ctx else_rhs in
    (* Check for combinational loop in combinational context *)
    if not ctx.in_sequential then begin
      if expr_depends_on ctx lhs_name then_rhs then
        failwith (Printf.sprintf "Combinational loop: %s depends on itself in then branch" lhs_name);
      if expr_depends_on ctx lhs_name else_rhs then
        failwith (Printf.sprintf "Combinational loop: %s depends on itself in else branch" lhs_name)
    end;
    
    let result_wire = gen_inst_name "wire" in
    let width = match get_var ctx lhs_name with
      | Some { width; _ } -> width
      | None -> max (infer_width ctx then_rhs) (infer_width ctx else_rhs)
    in
    ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (width-1) result_wire) :: !(ctx.wires);
    
    let _ = gen_mux ctx cond_wire else_wire then_wire result_wire width in
    
    (* Assign result to lhs *)
    structural_assign ctx lhs (VarRef { name = result_wire; access = "RD" }) ctx.in_sequential
  in
  
  List.iter process_var then_assigns
and structural_case ctx expr items =
  let expr_wire = structural_expr ctx expr in
  
  (* Collect all variables that are assigned *)
  let assigned_vars = ref [] in
  List.iter (fun item ->
    List.iter (function
      | Assign { lhs = VarRef { name; _ }; _ } ->
          if not (List.mem name !assigned_vars) then
            assigned_vars := name :: !assigned_vars
      | _ -> ()
    ) item.statements
  ) items;
  
  (* For each assigned variable, build proper mux tree *)
  List.iter (fun var_name ->
    (* Get actual width from variable table *)
    let width = match get_var ctx var_name with
      | Some { width; _ } -> width
      | None -> 
          add_warning (Printf.sprintf "Cannot determine width for %s, using 32" var_name);
          32
    in
    
    (* Build cascaded mux chain for this variable *)
    let rec build_mux_chain items default_val =
      match items with
      | [] -> default_val
      | item :: rest ->
          match item.conditions with
          | [] -> (* default case *)
              let case_val = List.find_map (function
                | Assign { lhs = VarRef { name; _ }; rhs; _ } when name = var_name -> Some rhs
                | _ -> None
              ) item.statements in
              (match case_val with
              | Some rhs -> structural_expr ctx rhs
              | None -> default_val)
          | cond :: _ ->
              let cond_wire = structural_expr ctx cond in
              let eq_wire = gen_inst_name "wire" in
              ctx.wires := (Printf.sprintf "  logic %s;" eq_wire) :: !(ctx.wires);
              let _ = gen_binary_op ctx "EQ" expr_wire cond_wire eq_wire 1 in
              
              (* Find value for this case *)
              let case_val = List.find_map (function
                | Assign { lhs = VarRef { name; _ }; rhs; _ } when name = var_name -> Some rhs
                | _ -> None
              ) item.statements in
              
              match case_val with
              | Some rhs ->
                  let rhs_wire = structural_expr ctx rhs in
                  let next_default = build_mux_chain rest default_val in
                  let result_wire = gen_inst_name "wire" in
                  ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (width-1) result_wire) :: !(ctx.wires);
                  let _ = gen_mux ctx eq_wire next_default rhs_wire result_wire width in
                  result_wire
              | None ->
                  build_mux_chain rest default_val
    in
    
    (* Start with default value based on context *)
    let default_wire = 
      if ctx.in_sequential then begin
        (* Sequential - can hold previous value *)
        var_name
      end else begin
        (* Combinational - must use constant to avoid loop *)
        let temp_wire = gen_inst_name "wire" in
        ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (width-1) temp_wire) :: !(ctx.wires);
        ctx.instances := (Printf.sprintf "  assign %s = %d'h0;" temp_wire width) :: !(ctx.instances);
        temp_wire
      end
    in
    
    let final_wire = build_mux_chain items default_wire in
    
    (* Only add assignment if final_wire is different from var_name *)
    if final_wire <> var_name then begin
      ctx.instances := 
        (Printf.sprintf "  assign %s = %s;" var_name final_wire) :: !(ctx.instances)
    end
  ) !assigned_vars

(* Flatten loop into unrolled statements *)
and flatten_loop ctx condition stmts incs max_iterations =
  add_warning (Printf.sprintf "Unrolling loop (max %d iterations)" max_iterations);
  
  (* Simple constant loop unrolling *)
  let rec unroll iter =
    if iter >= max_iterations then
      add_warning "Loop unrolling limit reached"
    else begin
      (* Execute body *)
      List.iter (structural_stmt ctx) stmts;
      
      (* Check condition (simplified - assumes we can evaluate) *)
      (* In real implementation, would need constant propagation *)
      
      (* Execute increments *)
      List.iter (structural_stmt ctx) incs;
      
      unroll (iter + 1)
    end
  in
  unroll 0

(* Convert statement to structural form *)
and structural_stmt ctx stmt =
  match stmt with
  | Assign { lhs; rhs; is_blocking = _ } ->
      structural_assign ctx lhs rhs ctx.in_sequential
  
  | AssignW { lhs; rhs } ->
      structural_assign ctx lhs rhs false
  
  | If { condition; then_stmt; else_stmt } ->
      structural_if ctx condition then_stmt else_stmt
  
  | Case { expr; items } ->
      structural_case ctx expr items
  
  | While { condition; stmts; incs } ->
      flatten_loop ctx condition stmts incs 64
  
  | Begin { stmts; _ } ->
      List.iter (structural_stmt ctx) stmts

  | Always { always; senses; stmts } ->
      (* NEW: Classify the block first *)
      let block_type = classify_always_block always senses stmts in
      
      (match block_type with
      | Unsynthesizable reason ->
          add_warning ("ERROR: Unsynthesizable always block: " ^ reason);
          failwith ("Cannot convert to structural: " ^ reason)
      
      | Sequential { clock; clock_edge; reset } ->
          (* Validate statements *)
          if not (List.for_all (validate_hardware_stmt ctx) stmts) then
            failwith "Sequential block contains unsynthesizable statements";
          
          (* Set context *)
          let old_sequential = ctx.in_sequential in
          let old_clock = ctx.current_clock in
          let old_reset = ctx.current_reset in
          
          ctx.in_sequential <- true;
          ctx.current_clock <- Some clock;
          ctx.current_reset <- (match reset with Some (r, _) -> Some r | None -> None);
          
          (* Convert statements *)
          List.iter (structural_stmt ctx) stmts;
          
          (* Restore context *)
          ctx.in_sequential <- old_sequential;
          ctx.current_clock <- old_clock;
          ctx.current_reset <- old_reset
      
      | Combinational ->
          if not (List.for_all (validate_hardware_stmt ctx) stmts) then
            failwith "Combinational block contains unsynthesizable statements";
          List.iter (structural_stmt ctx) stmts
      
      | Latch ->
	  add_warning "Converting always_latch block - ensure this is intentional";
	  (* For latches, we're more permissive - just process them *)
	  structural_latch ctx stmts)
    
  | Var { name; dtype_ref; dtype_name; var_type; _ } ->
      (match var_type with
      | "PORT" | "VAR" ->
          add_var ctx name dtype_ref dtype_name false
      | _ -> ())
  
  | _ -> ()

(* Inline function into expression *)
and inline_function ctx func_def args =
  add_warning (Printf.sprintf "Inlining function (not yet implemented)");
  VarRef { name = "/* inlined_func */"; access = "RD" }

(* Inline task into statements *)
and inline_task ctx task_def args =
  add_warning (Printf.sprintf "Inlining task (not yet implemented)");
  []
 
(* Validate that statements can be converted to hardware *)
and validate_hardware_stmt ctx = function
  | Assign _ | AssignW _ -> true

  | If { condition; then_stmt; else_stmt } ->
      let cond_valid = validate_hardware_expr ctx condition in
      let then_valid = validate_hardware_stmt ctx then_stmt in
      let else_valid = match else_stmt with 
        | Some s -> validate_hardware_stmt ctx s 
        | None -> true
      in
      cond_valid && then_valid && else_valid

  | Case { expr; items } ->
      validate_hardware_expr ctx expr &&
      List.for_all (fun { conditions; statements } ->
        List.for_all (validate_hardware_expr ctx) conditions &&
        List.for_all (validate_hardware_stmt ctx) statements
      ) items
  | Begin { stmts; _ } -> List.for_all (validate_hardware_stmt ctx) stmts
  | While _ -> false  (* Not synthesizable *)
  | EventCtrl _ | Delay _ | Initial _ | InitialStatic _ | Final _ -> false
  | Display _ | Finish | Stop _ -> false
  | _ -> true

and has_assignments = function
  | Assign _ | AssignW _ -> true
  | Begin { stmts; _ } -> List.exists has_assignments stmts
  | If { then_stmt; else_stmt; _ } ->
      has_assignments then_stmt || 
      (match else_stmt with Some s -> has_assignments s | None -> false)
  | _ -> false

and validate_hardware_expr ctx = function
  | VarRef _ | Const _ -> true
  | BinaryOp { lhs; rhs; _ } -> validate_hardware_expr ctx lhs && validate_hardware_expr ctx rhs
  | UnaryOp { operand; _ } -> validate_hardware_expr ctx operand
  | Cond { condition; then_val; else_val } ->
      validate_hardware_expr ctx condition &&
      validate_hardware_expr ctx then_val &&
      validate_hardware_expr ctx else_val
  | Sel { expr; lsb; width; _ } ->
      validate_hardware_expr ctx expr &&
      (match lsb with Some e -> validate_hardware_expr ctx e | None -> true) &&
      (match width with Some e -> validate_hardware_expr ctx e | None -> true)
  | ArraySel { expr; index } -> validate_hardware_expr ctx expr && validate_hardware_expr ctx index
  | Concat { parts } -> List.for_all (validate_hardware_expr ctx) parts
  | FuncRef { name; args } when name = "clog2" -> List.for_all (validate_hardware_expr ctx) args
  | FuncRef _ -> false  (* Most functions not synthesizable *)

  | Assign { lhs; rhs; is_blocking = _ } ->
      structural_assign ctx lhs rhs ctx.in_sequential; true
  
  | AssignW { lhs; rhs } ->
      structural_assign ctx lhs rhs false; true
  
  | If { condition; then_stmt; else_stmt } ->
      structural_if ctx condition then_stmt else_stmt; true
  
  | Case { expr; items } ->
      structural_case ctx expr items; true
  
  | While { condition; stmts; incs } ->
      flatten_loop ctx condition stmts incs 64; true
  
  | Begin { stmts; _ } ->
      List.iter (structural_stmt ctx) stmts; true
  
  | Var { name; dtype_ref; dtype_name; var_type; _ } ->
      (match var_type with
      | "PORT" | "VAR" ->
          add_var ctx name dtype_ref dtype_name false
      | _ -> ()); true
  
  | _ -> false

(* Convert always_latch block to structural latches *)
and structural_latch ctx stmts =
  add_warning "Converting always_latch block - ensure this is intentional";
  
  (* Try to detect enable signal from first statement *)
  let enable_sig = match stmts with
    | If { condition; then_stmt; _ } :: _ ->
        (match condition with
        | VarRef { name; _ } -> Some name
        | UnaryOp { op = "LOGNOT"; operand = VarRef { name; _ } } -> Some name
        | _ -> None)
    | _ -> None
  in
  
  (* Extract assignments from latch body *)
  let rec extract_latch_assigns = function
    | If { then_stmt; _ } -> extract_latch_assigns then_stmt
    | Assign { lhs; rhs; _ } -> [(lhs, rhs)]
    | Begin { stmts; _ } ->
        List.concat (List.map extract_latch_assigns stmts)
    | _ -> []
  in
  
  let assigns = List.concat (List.map extract_latch_assigns stmts) in
  
  (* Generate latch for each assignment *)
  List.iter (fun (lhs, rhs) ->
    let lhs_name = match lhs with
      | VarRef { name; _ } -> name
      | _ -> structural_expr ctx lhs
    in
    let rhs_wire = structural_expr ctx rhs in
    let width = match get_var ctx lhs_name with
      | Some { width; _ } -> width
      | None -> infer_width ctx rhs
    in
    
    let _ = gen_latch_en ctx enable_sig rhs_wire lhs_name width in
    ()
  ) assigns
  
(* Convert module to structural form - NOT part of mutual recursion *)
let structural_module name stmts =
  add_warning (Printf.sprintf "Converting module '%s' to structural form" name);
  let ctx = create_context () in
  
  (* First pass - collect variables *)
  List.iter (function
    | Var { name; dtype_ref; dtype_name; var_type; _ } ->
        let is_reg = var_type = "VAR" in
        add_var ctx name dtype_ref dtype_name is_reg
    | _ -> ()
  ) stmts;
  
  (* Second pass - convert logic *)
  List.iter (structural_stmt ctx) stmts;
  
  (* Generate output *)
  let ports = List.filter_map (function
    | Var { name; dtype_ref; dtype_name; var_type = "PORT"; direction; _ } ->
        let type_str = resolve_type_with_brackets dtype_name dtype_ref in
        Some (Printf.sprintf "  %s %s %s" (String.lowercase_ascii direction) type_str name)
    | _ -> None
  ) stmts in
  
  let port_str = String.concat ",\n" ports in
  let wires_str = String.concat "\n" (List.rev !(ctx.wires)) in
  let instances_str = String.concat "\n" (List.rev !(ctx.instances)) in
  
  Printf.sprintf "module %s (\n%s\n);\n\n%s\n\n%s\n\nendmodule" 
    name port_str wires_str instances_str

(* Main entry point *)
let rec generate_structural node indent =
  match node with
  | Netlist modules ->
      clear_warnings ();
      String.concat "\n\n" (List.map (fun m ->
        match m with
        | Module { name; stmts; _ } -> structural_module name stmts
        | Interface { name; _ } ->
            add_warning (Printf.sprintf "Skipping interface '%s'" name);
            ""
        | Package { name; stmts; _ } ->
            add_warning (Printf.sprintf "Skipping package '%s'" name);
            ""
        | _ -> ""
      ) modules)
  
  | Module { name; stmts; _ } ->
      structural_module name stmts
  
  | _ -> ""

(* Public API with warnings *)
let generate_structural_with_warnings ast =
  clear_warnings ();
  let result = generate_structural ast 0 in
  (result, get_warnings ())
