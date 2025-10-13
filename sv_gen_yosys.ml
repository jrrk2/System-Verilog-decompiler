open Sv_ast

let debug = ref false
let warnings = ref []

(* Add a warning message *)
let add_warning msg =
  warnings := msg :: !warnings;
  if !debug then Printf.eprintf "WARNING: %s\n" msg

(* Get all warnings *)
let get_warnings () = List.rev !warnings

(* Clear warnings *)
let clear_warnings () = warnings := []

let resolve_type = function
  | Some (RefType { name; _ }) -> name
  | Some (BasicType { keyword; range = Some r; _ }) -> keyword ^ " [" ^ r ^ "]"
  | Some (BasicType { keyword; range = None; _ }) -> keyword
  | Some (EnumType { name; _ }) -> name
  | _ -> "logic"

let rec resolve_array_type = function
  | Some (ArrayType { base; range }) ->
      let base_str = resolve_type (Some base) in
      Printf.sprintf "%s [%s]" base_str range
  | dtype -> resolve_type dtype

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

(* Extract interface references from statements *)
let extract_interface_refs stmts =
  if !debug then Printf.printf "DEBUG: Extracting interface refs from %d statements\n" (List.length stmts);
  let refs = List.fold_left (fun acc stmt ->
    match stmt with
    | Var { var_type = "IFACEREF"; dtype_ref = Some (IntfRefType { ifacename; modportname; modportp; _ }); name; _ } ->
        if !debug then Printf.printf "DEBUG: Found interface ref - var:%s iface:%s modport:%s\n" name ifacename modportname;
        (name, ifacename, modportname, modportp) :: acc
    | Var { var_type = "IFACEREF"; name; _ } ->
        if !debug then Printf.printf "DEBUG: Found IFACEREF but no IntfRefType for %s\n" name;
        acc
    | _ -> acc
  ) [] stmts in
  if !debug then Printf.printf "DEBUG: Total interface refs found: %d\n" (List.length refs);
  refs

(* Get modport directions from a modport definition *)
let get_modport_directions = function
  | Some (Modport { name; vars; _ }) -> 
      if !debug then Printf.printf "DEBUG: Getting directions from modport '%s' with %d vars\n" name (List.length vars);
      let dirs = List.fold_left (fun acc var ->
        match var with
        | ModportVarRef { name; direction; _ } -> 
            if !debug then Printf.printf "DEBUG: Modport var %s -> %s\n" name direction;
            (name, direction) :: acc
        | _ -> acc
      ) [] vars in
      if !debug then Printf.printf "DEBUG: Found %d modport directions\n" (List.length dirs);
      dirs
  | Some _ -> if !debug then Printf.printf "DEBUG: Unexpected definition found\n";
      []
  | None -> 
      if !debug then Printf.printf "DEBUG: No modport definition provided\n";
      []

(* Generate flattened ports for interface module *)
let generate_interface_ports iface_name modport_def interface_def =
  if !debug then Printf.printf "DEBUG: Generating ports for interface '%s'\n" iface_name;
  add_warning (Printf.sprintf "Flattening interface '%s' to individual ports for Yosys compatibility" iface_name);
  
  let directions = get_modport_directions modport_def in
  if !debug then Printf.printf "DEBUG: Modport directions: %d\n" (List.length directions);
  
  let interface_vars = match interface_def with
    | Some (Interface { stmts; _ }) -> 
        if !debug then Printf.printf "DEBUG: Interface has %d statements\n" (List.length stmts);
        stmts
    | _ -> 
        if !debug then Printf.printf "DEBUG: No interface definition\n";
        []
  in
  
  let ports = List.fold_left (fun acc (var_name, direction) ->
    if !debug then Printf.printf "DEBUG: Processing port %s (%s)\n" var_name direction;
    let var_info = List.find_opt (function
      | Var { name; _ } when name = var_name -> true
      | _ -> false
    ) interface_vars in
    
    match var_info with
    | Some (Var { dtype_ref; dtype_name; _ }) ->
        let var_type = resolve_type_with_brackets dtype_name dtype_ref in
        let port_dir = String.lowercase_ascii direction in
        let port_decl = Printf.sprintf "  %s %s %s" port_dir var_type var_name in
        if !debug then Printf.printf "DEBUG: Generated port: %s\n" port_decl;
        port_decl :: acc
    | _ -> 
        if !debug then Printf.printf "DEBUG: No var info found for %s\n" var_name;
        acc
  ) [] directions |> List.rev in
  if !debug then Printf.printf "DEBUG: Total ports generated: %d\n" (List.length ports);
  ports

(* Generate flattened signal declarations for top module *)
let generate_interface_signals iface_var_name interface_def =
  if !debug then Printf.printf "DEBUG: Generating signals for interface var '%s'\n" iface_var_name;
  match interface_def with
  | Some (Interface { stmts; _ }) ->
      if !debug then Printf.printf "DEBUG: Interface has %d statements\n" (List.length stmts);
      let signals = List.fold_left (fun acc stmt ->
        match stmt with
        | Var { name; dtype_ref; dtype_name; var_type = "VAR"; _ } ->
            let signal_type = resolve_type_with_brackets dtype_name dtype_ref in
            let flattened_name = Printf.sprintf "%s_%s" iface_var_name name in
            if !debug then Printf.printf "DEBUG: Generated signal: %s %s\n" signal_type flattened_name;
            (Printf.sprintf "  %s %s;" signal_type flattened_name) :: acc
        | _ -> acc
      ) [] stmts |> List.rev in
      if !debug then Printf.printf "DEBUG: Total signals generated: %d\n" (List.length signals);
      signals
  | _ -> []

(* Convert VarXRef to appropriate signal name *)
let convert_varxref_to_signal var_name dotted_name top_level_ports =
  match var_name with
  | name when List.mem name top_level_ports -> name
  | _ -> Printf.sprintf "%s_%s" dotted_name var_name

(* Generate port connections for module instantiation *)
let generate_port_connections iface_var_name modport_def interface_def top_level_ports =
  if !debug then Printf.printf "DEBUG: generate_port_connections called with iface_var_name=%s\n" iface_var_name;
  let directions = get_modport_directions modport_def in
  if !debug then Printf.printf "DEBUG: Found %d directions from modport\n" (List.length directions);
  
  let clock_conn = ".clk(clk)" in
  
  let other_conns = List.fold_left (fun acc (var_name, _) ->
    if var_name = "clk" then acc
    else
      let signal_name = Printf.sprintf "%s_%s" iface_var_name var_name in
      let conn = Printf.sprintf ".%s(%s)" var_name signal_name in
      if !debug then Printf.printf "DEBUG: Generated connection: %s\n" conn;
      conn :: acc
  ) [] directions in
  
  let result = clock_conn :: (List.rev other_conns) in
  if !debug then Printf.printf "DEBUG: Final connections: [%s]\n" (String.concat "; " result);
  result

(* Fix VarXRef nodes to use appropriate signal names *)
let fix_varxref_in_node top_level_ports iface_var_name node =
  let rec fix_node = function
    | VarXRef { name; dotted; _ } ->
        let signal_name = convert_varxref_to_signal name dotted top_level_ports in
        VarRef { name = signal_name; access = "RD" }
    | Assign { lhs; rhs; is_blocking } ->
        Assign { lhs = fix_node lhs; rhs = fix_node rhs; is_blocking }
    | BinaryOp { op; lhs; rhs } ->
        BinaryOp { op; lhs = fix_node lhs; rhs = fix_node rhs }
    | UnaryOp { op; operand } ->
        UnaryOp { op; operand = fix_node operand }
    | Always { always; senses; stmts } ->
        let fixed_senses = List.map fix_node senses in
        let fixed_stmts = List.map fix_node stmts in
        Always { always; senses = fixed_senses; stmts = fixed_stmts }
    | SenTree senses ->
        SenTree (List.map fix_node senses)
    | SenItem { edge_str; signal } ->
        SenItem { edge_str; signal = fix_node signal }
    | node -> node
  in fix_node node

(* Check if a statement is synthesizable *)
let rec is_synthesizable = function
  | Display _ -> 
      add_warning "Removing $display statement (simulation-only)";
      false
  | Initial _ -> 
      add_warning "Removing initial block (simulation-only)";
      false
  | InitialStatic _ -> 
      add_warning "Removing initial static block (simulation-only)";
      false
  | Final _ -> 
      add_warning "Removing final block (simulation-only)";
      false
  | Finish -> 
      add_warning "Removing $finish statement (simulation-only)";
      false
  | Stop _ -> 
      add_warning "Removing $stop/$fatal statement (simulation-only)";
      false
  | Delay _ -> 
      add_warning "Removing delay statement (simulation-only)";
      false
  | EventCtrl _ -> 
      add_warning "Removing event control (simulation-only)";
      false
  | Always { always; stmts; _ } when always = "always" ->
      (* Check if contains only simulation constructs *)
      if List.for_all (fun s -> not (is_synthesizable s)) stmts then begin
        add_warning "Removing always block containing only simulation constructs";
        false
      end else true
  | Always { always; _ } when always = "always_ff" || always = "always_comb" || always = "always_latch" ->
      true
  | TaskRef { name; _ } when String.get name 0 = '$' ->
      add_warning (Printf.sprintf "Removing system task %s (simulation-only)" name);
      false
  | FuncRef { name; _ } when name = "$random" || name = "$urandom" ->
      add_warning (Printf.sprintf "Removing random function %s (simulation-only)" name);
      false
  | If { then_stmt; else_stmt; _ } ->
      let then_synth = is_synthesizable then_stmt in
      let else_synth = match else_stmt with
        | Some s -> is_synthesizable s
        | None -> true
      in
      then_synth || else_synth
  | Begin { stmts; _ } ->
      List.exists is_synthesizable stmts
  | _ -> true

(* Filter out non-synthesizable statements *)
let filter_synthesizable stmts =
  List.filter is_synthesizable stmts

(* Binary operator conversion *)
let binary_op_to_string = function
  | "ADD" -> "+"
  | "SUB" -> "-"
  | "MUL" | "MULS" -> "*"
  | "DIV" | "DIVS" -> "/"
  | "POW" -> "**"
  | "POWSU" -> "**"
  | "AND" -> "&"
  | "OR" -> "|"
  | "XOR" -> "^"
  | "EQ" -> "=="
  | "EQWILD" -> "==?"
  | "NEQ" -> "!="
  | "NEQWILD" -> "!=?"
  | "NEQCASE" -> "!=="
  | "LT" | "LTS" -> "<"
  | "GT" | "GTS" -> ">"
  | "LTE" | "LTES" -> "<="
  | "GTE" | "GTES" -> ">="
  | "SHIFTL" -> "<<"
  | "SHIFTR" -> ">>"
  | "SHIFTRS" -> ">>>"
  | op -> op

(* Unary operator conversion *)
let unary_op_to_string = function
  | "NOT" -> "~"
  | "LOGNOT" -> "!"
  | "REDAND" -> "&"
  | "REDOR" -> "|"
  | "REDXOR" -> "^"
  | "NEGATE" -> "-"
  | "EXTEND" -> ""
  | "EXTENDS" -> "$signed"
  | "ISUNKNOWN" -> 
      add_warning "Using $isunknown (may not synthesize in all tools)";
      "$isunknown"
  | "ONEHOT" -> "$onehot"
  | "ONEHOT0" -> "$onehot0"
  | "CLOG2" -> "$clog2"
  | op -> op

(* Helper to extract struct members with proper types *)
let rec extract_struct_members = function
  | Some (StructType { members; _ }) ->
      List.filter_map (function
        | MemberType { name; dtype_ref; _ } ->
            let typ = match dtype_ref with
              | Some (BasicType { keyword; range = Some r; _ }) -> 
                  Printf.sprintf "%s [%s]" keyword r
              | Some (BasicType { keyword; range = None; _ }) -> keyword
              | Some (RefType { name = tname; _ }) -> tname
              | None -> "logic"
              | _ -> "logic"
            in
            Some (Printf.sprintf "    %s %s;" typ name)
        | _ -> None
      ) members
  | _ -> []

let parse_verilog_const s =
  try
    let s = String.trim s in
    if not (String.contains s '\'') then
      int_of_string s
    else
      let parts = Str.split (Str.regexp "'") s in
      match parts with
      | [width; value_part] when String.length value_part > 0 ->
          (match String.get value_part 0 with
          | 'h' -> int_of_string ("0x" ^ String.sub value_part 1 (String.length value_part - 1))
          | 'd' -> int_of_string (String.sub value_part 1 (String.length value_part - 1))
          | 's' when String.length value_part > 1 ->
              (match String.get value_part 1 with
              | 'h' -> int_of_string ("0x" ^ String.sub value_part 2 (String.length value_part - 2))
              | 'd' -> int_of_string (String.sub value_part 2 (String.length value_part - 2))
              | _ -> 0)
          | 'b' -> int_of_string ("0b" ^ String.sub value_part 1 (String.length value_part - 1))
          | c when c >= '0' && c <= '9' -> int_of_string value_part
          | _ -> 0)
      | [num] -> int_of_string num
      | _ -> 0
  with _ -> 0

let rec generate_sv node indent =
  let ind = String.make (indent * 2) ' ' in
  match node with
  | Netlist modules ->
      let interfaces = List.filter_map (function
        | Interface _ as iface -> Some iface
        | _ -> None
      ) modules in
      String.concat "\n\n" (List.map (generate_sv_with_interfaces_indent 0 interfaces) modules)

  | Module { name; stmts; _ } ->
      Printf.printf "ERROR: generate_sv called on Module without interface context\n";
      generate_regular_module name stmts []

  | Interface { name; _ } ->
      add_warning (Printf.sprintf "Interface '%s' will be flattened into module ports" name);
      ""

  | Cell { name; modp_addr; pins; _ } ->
      (match modp_addr with
      | Some (Interface { name = iface_name; _ }) ->
          ""
      | Some (Module { name = module_name; _ } as mod_def) ->
          let connections = generate_cell_connections pins (Some mod_def) [] in
          if List.length connections > 0 then
            Printf.sprintf "%s%s %s (\n%s%s\n%s);" ind module_name name 
              (String.make ((indent + 1) * 2) ' ')
              (String.concat (",\n" ^ String.make ((indent + 1) * 2) ' ') connections)
              ind
          else
            Printf.sprintf "%s%s %s ();" ind module_name name
      | _ -> Printf.sprintf "%s// Unknown cell %s" ind name)

  | Var { name; dtype_ref; var_type; direction; value; dtype_name; is_param; _ } ->
      let resolved_type = resolve_type_with_brackets dtype_name dtype_ref in
      let array_range = match dtype_ref with
        | Some (ArrayType { range; _ }) -> 
            if String.contains range ':' then
              " [" ^ range ^ "]"
            else
              let upper_bound = parse_verilog_const range in
              Printf.sprintf " [0:%d]" upper_bound
        | _ -> ""
      in
      
      let prefix = match var_type with
        | "LPARAM" -> "localparam"
        | "GPARAM" -> "parameter"
        | "PORT" -> String.lowercase_ascii direction
        | "GENVAR" -> "genvar"
        | _ -> if is_param then "parameter" else ""
      in
      let val_str = match value with
        | Some v -> " = " ^ (generate_sv_with_interfaces v 0 [] |> String.trim)
        | None -> ""
      in
      if var_type = "PORT" then
        Printf.sprintf "%s %s %s%s%s" prefix resolved_type name array_range val_str
      else if prefix <> "" then
        Printf.sprintf "%s%s %s %s%s%s;" ind prefix resolved_type name array_range val_str
      else
        Printf.sprintf "%s%s %s%s%s;" ind resolved_type name array_range val_str

  | Always { always; senses; stmts; _ } ->
      let synth_stmts = filter_synthesizable stmts in
      if List.length synth_stmts = 0 then "" else
      let sense_str = match senses with
        | [] -> ""
        | _ -> " " ^ (String.concat "" (List.map (generate_sv_with_interfaces_indent 0 []) senses))
      in
      let stmt_str = String.concat "\n" (List.map (generate_sv_with_interfaces_indent (indent + 1) []) synth_stmts) in
      Printf.sprintf "%s%s%s begin\n%s\n%send" ind always sense_str stmt_str ind

  | Assign { lhs; rhs; is_blocking; _ } ->
      let op = if is_blocking then "=" else "<=" in
      Printf.sprintf "%s%s %s %s;" ind 
        (generate_sv_with_interfaces lhs 0 [] |> String.trim) op (generate_sv_with_interfaces rhs 0 [] |> String.trim)

  | AssignW { lhs; rhs; _ } ->
      Printf.sprintf "%sassign %s = %s;" ind 
        (generate_sv_with_interfaces lhs 0 [] |> String.trim) (generate_sv_with_interfaces rhs 0 [] |> String.trim)

  | VarRef { name; _ } -> name

  | VarXRef { name; dotted; _ } ->
      Printf.sprintf "%s.%s" dotted name

  | SenTree senses ->
      Printf.sprintf "@(%s)" 
        (String.concat " or " (List.map (generate_sv_with_interfaces_indent 0 []) senses))

  | SenItem { edge_str; signal; _ } ->
      Printf.sprintf "%s %s" edge_str (generate_sv_with_interfaces signal 0 [] |> String.trim)

  | BinaryOp { op; lhs; rhs; _ } ->
      let op_str = binary_op_to_string op in
      Printf.sprintf "(%s %s %s)" 
        (generate_sv_with_interfaces lhs 0 [] |> String.trim) op_str (generate_sv_with_interfaces rhs 0 [] |> String.trim)

  | UnaryOp { op; operand; _ } ->
      let op_str = unary_op_to_string op in
      if op_str = "" then
        generate_sv_with_interfaces operand 0 []
      else if String.contains op_str '$' then
        Printf.sprintf "%s(%s)" op_str (generate_sv_with_interfaces operand 0 [] |> String.trim)
      else
        Printf.sprintf "%s(%s)" op_str (generate_sv_with_interfaces operand 0 [] |> String.trim)

  | Const { name; _ } -> name

  | Begin { name; stmts; is_generate; _ } ->
      let synth_stmts = filter_synthesizable stmts in
      if List.length synth_stmts = 0 then "" else
      let stmt_str = String.concat "\n" (List.map (generate_sv_with_interfaces_indent (indent + 1) []) synth_stmts) in

      if String.contains name '[' then
        Printf.sprintf "%s// Generate instance %s\n%s" ind name stmt_str
      else if name <> "" && not (String.contains name '$') && name <> "unnamedblk1" && not (Str.string_match (Str.regexp ".*unnamedblk.*") name 0) then
        if is_generate then
          Printf.sprintf "%sbegin : %s\n%s\n%send" ind name stmt_str ind
        else
          Printf.sprintf "%sbegin : %s\n%s\n%send" ind name stmt_str ind
      else
        Printf.sprintf "%sbegin\n%s\n%send" ind stmt_str ind

  | If { condition; then_stmt; else_stmt; _ } ->
      let cond_str = generate_sv_with_interfaces condition 0 [] |> String.trim in
      let then_str = generate_sv_with_interfaces then_stmt indent [] in
      let else_str = match else_stmt with
        | Some stmt -> 
            let else_code = generate_sv_with_interfaces stmt indent [] in
            if else_code = "" then "" else Printf.sprintf " else\n%s" else_code
        | None -> ""
      in
      if then_str = "" && else_str = "" then "" else
      Printf.sprintf "%sif (%s)\n%s%s" ind cond_str then_str else_str

  | Case { expr; items; _ } ->
      let expr_str = generate_sv_with_interfaces expr 0 [] |> String.trim in
      let items_str = String.concat "\n" 
        (List.filter_map (fun item ->
          let synth_statements = filter_synthesizable item.statements in
          if List.length synth_statements = 0 then None else
          match item.conditions with
          | [] -> 
              let stmt_str = String.concat "\n" 
                (List.map (generate_sv_with_interfaces_indent (indent + 2) []) synth_statements) in
              Some (Printf.sprintf "%s  default: begin\n%s\n%s  end" ind stmt_str ind)
          | conds ->
              let cond_str = String.concat ", " 
                (List.map (fun c -> generate_sv_with_interfaces c 0 [] |> String.trim) conds) in
              let stmt_str = String.concat "\n" 
                (List.map (generate_sv_with_interfaces_indent (indent + 2) []) synth_statements) in
              Some (Printf.sprintf "%s  %s: begin\n%s\n%s  end" ind cond_str stmt_str ind)
        ) items) in
      if items_str = "" then "" else
      Printf.sprintf "%scase (%s)\n%s\n%sendcase" ind expr_str items_str ind

  | While { condition; stmts; incs; _ } ->
      add_warning "While loop found - ensure it's in a generate context or testbench";
      let cond_str = generate_sv_with_interfaces condition 0 [] |> String.trim in
      let stmt_str = String.concat "\n" (List.map (generate_sv_with_interfaces_indent (indent + 1) []) stmts) in
      let inc_str = String.concat "\n" (List.map (generate_sv_with_interfaces_indent (indent + 1) []) incs) in
      if List.length incs > 0 then
        Printf.sprintf "%sfor (; %s; ) begin\n%s\n%s\n%send" ind cond_str stmt_str inc_str ind
      else
        Printf.sprintf "%swhile (%s) begin\n%s\n%send" ind cond_str stmt_str ind

  | Sel { expr; lsb; width; range; _ } ->
      let base = generate_sv_with_interfaces expr 0 [] |> String.trim in
      (match lsb, width with
      | Some l, Some w ->
          let lsb_str = generate_sv_with_interfaces l 0 [] |> String.trim in
          let width_str = generate_sv_with_interfaces w 0 [] |> String.trim in
          Printf.sprintf "%s[%s +: %s]" base lsb_str width_str
      | Some l, None ->
          (match l with
          | Const { name = cname; _ } -> 
              let const_val = parse_verilog_const cname in
              if const_val = 0 && width = None then
                base
              else
                let lsb_str = generate_sv_with_interfaces l 0 [] |> String.trim in
                Printf.sprintf "%s[%s]" base lsb_str
          | _ -> 
              let lsb_str = generate_sv_with_interfaces l 0 [] |> String.trim in
              Printf.sprintf "%s[%s]" base lsb_str)
      | None, None when range <> "" -> Printf.sprintf "%s[%s]" base range
      | _ -> base)

  | ArraySel { expr; index; _ } ->
      Printf.sprintf "%s[%s]" 
        (generate_sv_with_interfaces expr 0 [] |> String.trim)
        (generate_sv_with_interfaces index 0 [] |> String.trim)

  | FuncRef { name; args; _ } ->
      let arg_str = String.concat ", " 
        (List.map (fun a -> generate_sv_with_interfaces a 0 [] |> String.trim) args) in
      Printf.sprintf "%s(%s)" name arg_str

  | TaskRef { name; args; _ } ->
      let arg_str = String.concat ", "
        (List.map (fun a -> generate_sv_with_interfaces a 0 [] |> String.trim) args) in
      Printf.sprintf "%s%s(%s);" ind name arg_str

  | Concat { parts; _ } ->
      let parts_str = String.concat ", "
        (List.map (fun p -> generate_sv_with_interfaces p 0 [] |> String.trim) parts) in
      Printf.sprintf "{%s}" parts_str

  | Cond { condition; then_val; else_val; _ } ->
      Printf.sprintf "(%s ? %s : %s)"
        (generate_sv_with_interfaces condition 0 [] |> String.trim)
        (generate_sv_with_interfaces then_val 0 [] |> String.trim)
        (generate_sv_with_interfaces else_val 0 [] |> String.trim)

  | Package { name; stmts; _ } ->
      let synth_stmts = filter_synthesizable stmts in
      let internal_str = String.concat "\n\n" 
        (List.map (generate_sv_with_interfaces_indent 1 []) synth_stmts) in
      Printf.sprintf "package %s;\n%s\nendpackage" name internal_str

  | Typedef { name; dtype_ref; _ } ->
      let type_str = match dtype_ref with
        | Some (EnumType { items; _ }) ->
            let items_str = String.concat ",\n        " 
              (List.map (fun (n, v) -> Printf.sprintf "%s = %s" n v) items) in
            Printf.sprintf "typedef enum logic [1:0] {\n        %s\n    } %s;" items_str name
        | Some (StructType { packed; members; name = struct_name; _ }) ->
            let pack_str = if packed then "packed " else "" in
            let members_str = extract_struct_members dtype_ref in
            if List.length members_str > 0 then
              Printf.sprintf "typedef %sstruct {\n%s\n    } %s;" pack_str 
                (String.concat "\n" members_str) name
            else
              Printf.sprintf "typedef %sstruct {\n        // members\n    } %s;" pack_str name
        | Some typ -> Printf.sprintf "typedef %s %s;" (resolve_type (Some typ)) name
        | None -> Printf.sprintf "typedef ... %s;" name
      in
      Printf.sprintf "%s%s" ind type_str

  | Func { name; dtype_ref; stmts; vars; _ } ->
      let synth_stmts = filter_synthesizable stmts in
      let return_type = resolve_type dtype_ref in

      let inputs = List.filter (function
        | Var { var_type = "PORT"; direction = "INPUT"; _ } -> true
        | _ -> false
      ) vars in

      let params_str = 
        if List.length inputs > 0 then
          let params = List.map (fun v ->
            match v with
            | Var { name = vname; dtype_ref = vtype; dtype_name; _ } ->
                let vtype_str = resolve_type_with_brackets dtype_name vtype in
                Printf.sprintf "input %s %s" vtype_str vname
            | _ -> ""
          ) inputs in
          String.concat ", " params
        else
          ""
      in

      let body_str = String.concat "\n" 
        (List.map (generate_sv_with_interfaces_indent (indent + 2) []) synth_stmts) in

      Printf.sprintf "%sfunction automatic %s %s(%s);\n%s\n%sendfunction" 
        ind return_type name params_str body_str ind

  | Task { name; dtype_ref; stmts; vars; _ } ->
      let synth_stmts = filter_synthesizable stmts in
      let inputs = List.filter (function
        | Var { var_type = "PORT"; _ } -> true
        | _ -> false
      ) vars in

      let params_str = 
        if List.length inputs > 0 then
          let params = List.map (fun v ->
            match v with
            | Var { name = vname; dtype_ref = vtype; dtype_name; direction; _ } ->
                let vtype_str = resolve_type_with_brackets dtype_name vtype in
                let dir_str = String.lowercase_ascii direction in
                Printf.sprintf "%s %s %s" dir_str vtype_str vname
            | _ -> ""
          ) inputs in
          String.concat ", " params
        else
          ""
      in

      let body_str = String.concat "\n"
        (List.map (generate_sv_with_interfaces_indent (indent + 2) []) synth_stmts) in

      Printf.sprintf "%stask automatic %s(%s);\n%s\n%sendtask"
      ind name params_str body_str ind

  | CaseItem { conditions; stmts; _ } ->
      let synth_stmts = filter_synthesizable stmts in
      let cond_str = String.concat ", "
        (List.map (fun c -> generate_sv_with_interfaces c 0 [] |> String.trim) conditions) in
      let stmt_str = String.concat "\n"
        (List.map (generate_sv_with_interfaces_indent indent []) synth_stmts) in
      if conditions = [] then
        Printf.sprintf "%sdefault: begin\n%s\n%send" ind stmt_str ind
      else
        Printf.sprintf "%s%s: begin\n%s\n%send" ind cond_str stmt_str ind

  | Pin { name; expr; _ } ->
      (match expr with
      | Some e -> Printf.sprintf ".%s(%s)" name (generate_sv_with_interfaces e 0 [] |> String.trim)
      | None -> Printf.sprintf ".%s()" name)

  | Modport { name; vars; _ } ->
      let var_strs = List.map (fun v ->
        match v with
        | ModportVarRef { name; direction; _ } ->
            Printf.sprintf "    %s %s" (String.lowercase_ascii direction) name
        | _ -> ""
      ) vars in
      Printf.sprintf "%smodport %s (\n%s\n%s);" ind name
        (String.concat ",\n" var_strs) ind

  | ModportVarRef { name; direction; _ } ->
      Printf.sprintf "%s %s" (String.lowercase_ascii direction) name

  | Replicate { dtype_ref; src; count; _ } ->
      Printf.sprintf "{%s{%s}}"
        (generate_sv_with_interfaces count 0 [] |> String.trim)
        (generate_sv_with_interfaces src 0 [] |> String.trim)

  | InsideRange { lhs; rhs; _ } ->
      Printf.sprintf "%s inside {%s}"
        (generate_sv_with_interfaces lhs 0 [] |> String.trim)
        (generate_sv_with_interfaces rhs 0 [] |> String.trim)

  | ConsPack { dtype_ref; members; _ } ->
      let members_str = String.concat ", "
        (List.map (generate_sv_with_interfaces_indent 0 []) members) in
      Printf.sprintf "'{%s}" members_str

  | ConsPackMember { dtype_ref; rhs; _ } ->
      generate_sv_with_interfaces rhs 0 []

  | InitArray { inits; _ } ->
      let init_str = String.concat ", "
        (List.map (generate_sv_with_interfaces_indent 0 []) inits) in
      Printf.sprintf "'{%s}" init_str

  | InitItem { value; _ } ->
      let val_str = String.concat ", "
        (List.map (generate_sv_with_interfaces_indent 0 []) value) in
      val_str

  | JumpBlock { stmt; _ } ->
      let synth_stmts = filter_synthesizable stmt in
      String.concat "\n" (List.map (generate_sv_with_interfaces_indent indent []) synth_stmts)

  (* Simulation-only constructs - filtered out *)
  | Display _ | Initial _ | InitialStatic _ | Final _ 
  | Finish | Stop _ | Delay _ | EventCtrl _ -> ""
  
  (* Other constructs that might appear *)
  | Sformatp _ | Text _ | Time _ | StmtExpr _ 
  | Cexpr _ | Sampled _ | ScopeName _ 
  | Itord _ | CvtPackString _ | Fopen _ | Fclose _
  | ValuePlusArgs _ | TestPlusArgs _ | CMethodHard _ | JumpGo _ -> ""

  | _ -> "/* Unhandled node */"

and generate_sv_indent indent node = generate_sv node indent

and generate_sv_with_interfaces node indent interfaces =
  match node with
  | Module { name; stmts; _ } ->
      let interface_refs = extract_interface_refs stmts in
      let has_regular_ports = List.exists (function | Var { var_type = "PORT"; _ } -> true | _ -> false) stmts in
      
      if has_regular_ports then
        generate_top_module_with_interfaces name stmts interface_refs interfaces
      else if interface_refs <> [] then
        generate_interface_module_with_interfaces name stmts interface_refs interfaces
      else
        generate_regular_module name stmts []
  
  | _ -> generate_sv node indent

and generate_sv_with_interfaces_indent indent interfaces node = 
  generate_sv_with_interfaces node indent interfaces

and generate_top_module_with_interfaces name stmts interface_refs interfaces =
  if !debug then Printf.printf "DEBUG: Generating top module '%s' with %d interface refs\n" name (List.length interface_refs);
  
  let top_level_ports = List.fold_left (fun acc stmt ->
    match stmt with
    | Var { var_type = "PORT"; name; _ } -> name :: acc
    | _ -> acc
  ) [] stmts in
  
  (* Separate module parameters and regular statements *)
  let (params, regular_ports, cells, gen_blocks, other_stmts) = 
    List.fold_left (fun (ps, ports, cells, gens, others) stmt ->
      match stmt with
      | Var { var_type = "GPARAM"; _ } as v -> (v :: ps, ports, cells, gens, others)
      | Var { var_type = "PORT"; _ } as v -> (ps, v :: ports, cells, gens, others)
      | Var { var_type = "IFACEREF"; _ } -> (ps, ports, cells, gens, others)
      | Cell _ as c -> (ps, ports, c :: cells, gens, others)
      | Always _ as a when is_synthesizable a -> (ps, ports, cells, gens, a :: others)
      | Begin { name = bname; is_generate = true; _ } as b ->
          (ps, ports, cells, b :: gens, others)
      | stmt when is_synthesizable stmt -> (ps, ports, cells, gens, stmt :: others)
      | _ -> (ps, ports, cells, gens, others)
    ) ([], [], [], [], []) stmts in

  (* Generate module header with parameters *)
  let param_str = if List.length params > 0 then
    let param_decls = List.map (fun p -> 
      match p with
      | Var { name; dtype_ref; dtype_name; value; _ } ->
          let resolved_type = resolve_type_with_brackets dtype_name dtype_ref in
          let val_str = match value with
            | Some v -> " = " ^ (generate_sv_with_interfaces v 0 [] |> String.trim)
            | None -> ""
          in
          Printf.sprintf "  parameter %s %s%s" resolved_type name val_str
      | _ -> ""
    ) (List.rev params) in
    " #(\n" ^ String.concat ",\n" param_decls ^ "\n)"
  else
    ""
  in

  (* Keep ports in original order *)
  let port_decls = List.map (fun v -> generate_sv_with_interfaces v 1 interfaces) (List.rev regular_ports) in
  
  let signal_decls = List.fold_left (fun acc (var_name, iface_name, _, _) ->
    let interface_def = find_interface_by_name iface_name interfaces in
    let signals = generate_interface_signals var_name interface_def in
    signals @ acc
  ) [] interface_refs in

  let cell_stmts = List.map (fun cell ->
    match cell with
    | Cell { name; modp_addr; pins; _ } ->
        (match modp_addr with
        | Some (Module { name = module_name; stmts = mod_stmts; _ }) ->
            let mod_interface_refs = extract_interface_refs mod_stmts in
            (match mod_interface_refs with
            | (_, iface_name, _, modportp) :: _ ->
                let interface_def = find_interface_by_name iface_name interfaces in
                let iface_var_name = match interface_refs with
                  | (var_name, _, _, _) :: _ -> var_name
                  | [] -> "sif"
                in
                let connections = generate_port_connections iface_var_name modportp interface_def top_level_ports in
                Printf.sprintf "  %s %s (%s);" module_name name (String.concat ", " connections)
            | [] ->
                let connections = generate_cell_connections pins (Some (Module { name = module_name; stmts = mod_stmts })) [] in
                if List.length connections > 0 then
                  Printf.sprintf "  %s %s (\n    %s\n  );" module_name name 
                    (String.concat ",\n    " connections)
                else
                  Printf.sprintf "  %s %s ();" module_name name)
        | Some (Interface _) -> ""
        | _ -> "  // Unknown cell")
    | _ -> ""
  ) cells in

  (* Process generate blocks *)
  let gen_stmts = List.map (generate_sv_with_interfaces_indent 1 interfaces) (List.rev gen_blocks) in

  let other_assigns = List.filter_map (fun stmt ->
    match stmt with
    | AssignW { lhs; rhs } ->
        let fixed_rhs = match rhs with
          | VarXRef { name; dotted; _ } -> 
              let signal_name = Printf.sprintf "%s_%s" dotted name in
              VarRef { name = signal_name; access = "RD" }
          | _ -> rhs
        in
        let fixed_lhs = match lhs with
          | VarRef { name; _ } when List.mem name top_level_ports -> lhs
          | _ -> fix_varxref_in_node top_level_ports "" lhs
        in
        Some (generate_sv_with_interfaces (AssignW { lhs = fixed_lhs; rhs = fixed_rhs }) 1 interfaces)
    | stmt -> 
        let code = generate_sv_with_interfaces stmt 1 interfaces in
        if code = "" then None else Some code
  ) other_stmts in

  let port_str = String.concat ",\n" port_decls in
  let body_parts = signal_decls @ cell_stmts @ gen_stmts @ other_assigns in
  let body = String.concat "\n" (List.filter (fun s -> s <> "") body_parts) in

  Printf.sprintf "module %s%s (\n%s\n);\n%s\nendmodule" name param_str port_str body

and generate_interface_module_with_interfaces name stmts interface_refs interfaces =
  match interface_refs with
  | (var_name, iface_name, modport_name, modportp) :: _ ->
      let interface_def = find_interface_by_name iface_name interfaces in
      let flattened_ports = generate_interface_ports var_name modportp interface_def in
      let port_str = String.concat ",\n" flattened_ports in
      
      let module_port_names = get_modport_directions modportp |> List.map fst in
      
      let internals = List.filter (function
        | Var { var_type = "IFACEREF"; _ } -> false
        | stmt -> is_synthesizable stmt
      ) stmts in
      
      let fixed_internals = List.map (fix_varxref_in_node module_port_names var_name) internals in
      let internal_str = String.concat "\n" (List.filter_map (fun stmt ->
        let code = generate_sv_with_interfaces_indent 1 interfaces stmt in
        if code = "" then None else Some code
      ) fixed_internals) in
      
      Printf.sprintf "module %s (\n%s\n);\n%s\nendmodule" name port_str internal_str
  | [] -> generate_regular_module name stmts []

and generate_regular_module name stmts top_level_ports =
  let ports = List.filter_map (function
    | Var { var_type = "PORT"; _ } as v -> Some (generate_sv v 1 |> String.trim)
    | _ -> None
  ) stmts in
  
  let internals = List.filter (function
    | Var { var_type = "PORT"; _ } -> false
    | stmt -> is_synthesizable stmt
  ) stmts in
  
  let port_str = if ports = [] then "" else Printf.sprintf " (\n  %s\n)" (String.concat ",\n  " ports) in
  let internal_str = String.concat "\n" (List.filter_map (fun stmt ->
    let code = generate_sv_indent 1 stmt in
    if code = "" then None else Some code
  ) internals) in
  
  Printf.sprintf "module %s%s;\n%s\nendmodule" name port_str internal_str

and find_interface_by_name iface_name interfaces =
  List.find_opt (function
    | Interface { name; _ } when name = iface_name -> true
    | _ -> false
  ) interfaces

and extract_iface_var_name_from_pins pins =
  match pins with
  | Pin { expr = Some (VarRef { name; _ }); _ } :: _ when name <> "clk" -> name
  | _ -> "sif"

and generate_cell_connections pins module_def top_level_ports =
  List.filter_map (fun pin ->
    match pin with
    | Pin { name; expr = Some e } ->
        let expr_str = generate_sv_with_interfaces e 0 [] |> String.trim in
        Some (Printf.sprintf ".%s(%s)" name expr_str)
    | Pin { name; expr = None } ->
        Some (Printf.sprintf ".%s()" name)
    | _ -> None
  ) pins

(* Public API for getting warnings *)
let generate_sv_with_warnings ast indent =
  clear_warnings ();
  let result = generate_sv ast indent in
  (result, get_warnings ())