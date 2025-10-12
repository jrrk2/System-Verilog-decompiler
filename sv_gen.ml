open Sv_ast

let debug = ref false

let resolve_type = function
  | Some (RefType { name; _ }) -> name
  | Some (BasicType { keyword; range = Some r; _ }) -> keyword ^ " [" ^ r ^ "]"
  | Some (BasicType { keyword; range = None; _ }) -> keyword
  | Some (EnumType { name; _ }) -> name
  | _ -> "logic"

(* Resolve array type with proper range syntax *)
let rec resolve_array_type = function
  | Some (ArrayType { base; range }) ->
      let base_str = resolve_type (Some base) in
      Printf.sprintf "%s [%s]" base_str range
  | dtype -> resolve_type dtype

let resolve_type_with_brackets dtype_name dtype_ref =
  (* Handle array types specially - the range goes AFTER the variable name *)
  match dtype_ref with
  | Some (ArrayType _) ->
      (* For arrays, return just the base type; range handled separately *)
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
  List.iter (fun (name, dir) -> if !debug then Printf.printf "DEBUG: Direction %s -> %s\n" name dir) directions;
  
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
  | "NOT" -> "!"
  | "LOGNOT" -> "!"
  | "REDAND" -> "&"
  | "REDOR" -> "|"
  | "REDXOR" -> "^"
  | "NEGATE" -> "-"
  | "EXTEND" -> ""
  | "EXTENDS" -> "$signed"
  | "ISUNKNOWN" -> "$isunknown"
  | "ONEHOT" -> "$onehot"
  | "ONEHOT0" -> "$onehot0"
  | "CLOG2" -> "$clog2"
  | op -> op

(* Helper to extract struct members with proper types *)
let rec extract_struct_members = function
  | Some (StructType { members; _ }) ->
      List.filter_map (function
        | MemberType { name; dtype_ref; _ } ->
            (* Use dtype_ref instead of child *)
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
      ""

  | Cell { name; modp_addr; pins; _ } ->
      (match modp_addr with
      | Some (Interface { name = iface_name; _ }) ->
          ""
      | Some (Module { name = module_name; _ } as mod_def) ->
          let connections = generate_cell_connections pins (Some mod_def) [] in
          Printf.sprintf "%s%s %s (%s);" ind module_name name (String.concat ", " connections)
      | _ -> Printf.sprintf "%s// Unknown cell %s" ind name)

  | Var { name; dtype_ref; var_type; direction; value; dtype_name; is_param; _ } ->
      let resolved_type = resolve_type_with_brackets dtype_name dtype_ref in

      (* Extract array range if present *)
      let array_range = match dtype_ref with
        | Some (ArrayType { range; _ }) -> 
            if String.contains range ':' then
              " [" ^ range ^ "]"
            else
              let parse_verilog_const s =
                try
                  let parts = Str.split (Str.regexp "'") s in
                  match parts with
                  | [_; rest] ->
                      let value_str = if String.length rest > 0 then
                        String.sub rest 1 (String.length rest - 1)
                      else "0" in
                      if String.get rest 0 = 'h' then
                        int_of_string ("0x" ^ value_str)
                      else if String.get rest 0 = 'd' then
                        int_of_string value_str
                      else
                        int_of_string value_str
                  | _ -> 0
                with _ -> 255
              in
              let upper = parse_verilog_const range in
              let size = if upper = 0 then 255 else upper in
              Printf.sprintf " [0:%d]" size
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
      let sense_str = match senses with
        | [] -> ""
        | _ -> " " ^ (String.concat "" (List.map (generate_sv_with_interfaces_indent 0 []) senses))
      in
      let stmt_str = String.concat "\n" (List.map (generate_sv_with_interfaces_indent (indent + 1) []) stmts) in
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
        (* System function style *)
        Printf.sprintf "%s(%s)" op_str (generate_sv_with_interfaces operand 0 [] |> String.trim)
      else
        Printf.sprintf "%s(%s)" op_str (generate_sv_with_interfaces operand 0 [] |> String.trim)

  | Const { name; _ } -> name

  | Begin { name; stmts; is_generate; _ } ->
      let stmt_str = String.concat "\n" (List.map (generate_sv_with_interfaces_indent (indent + 1) []) stmts) in

      let has_indexed_children = List.exists (function
        | Begin { name = child_name; _ } when String.contains child_name '[' -> true
        | _ -> false
      ) stmts in

      if name <> "" && not (String.contains name '$') && 
         not (String.contains name '[') &&
         name <> "unnamedblk1" then
        if is_generate then
          if has_indexed_children && List.length stmts > 0 then
            Printf.sprintf "%sgenerate\n%s  for (genvar i = 0; i < 2; i++) begin : %s\n%s\n%s  end\n%sendgenerate" 
              ind ind name stmt_str ind ind
          else
            Printf.sprintf "%sbegin : %s\n%s\n%send" ind name stmt_str ind
        else
          Printf.sprintf "%sbegin : %s\n%s\n%send" ind name stmt_str ind
      else if String.contains name '[' then
        Printf.sprintf "%s// Generate block instance %s\n%s" ind name stmt_str
      else
        Printf.sprintf "%sbegin\n%s\n%send" ind stmt_str ind

  | If { condition; then_stmt; else_stmt; _ } ->
      let cond_str = generate_sv_with_interfaces condition 0 [] |> String.trim in
      let then_str = generate_sv_with_interfaces then_stmt indent [] in
      let else_str = match else_stmt with
        | Some stmt -> 
            Printf.sprintf " else\n%s" (generate_sv_with_interfaces stmt indent [])
        | None -> ""
      in
      Printf.sprintf "%sif (%s)\n%s%s" ind cond_str then_str else_str

  | Case { expr; items; _ } ->
      let expr_str = generate_sv_with_interfaces expr 0 [] |> String.trim in
      let items_str = String.concat "\n" 
        (List.map (fun item ->
          match item.conditions with
          | [] -> 
              let stmt_str = String.concat "\n" 
                (List.map (generate_sv_with_interfaces_indent (indent + 2) []) item.statements) in
              Printf.sprintf "%s  default: begin\n%s\n%s  end" ind stmt_str ind
          | conds ->
              let cond_str = String.concat ", " 
                (List.map (fun c -> generate_sv_with_interfaces c 0 [] |> String.trim) conds) in
              let stmt_str = String.concat "\n" 
                (List.map (generate_sv_with_interfaces_indent (indent + 2) []) item.statements) in
              Printf.sprintf "%s  %s: begin\n%s\n%s  end" ind cond_str stmt_str ind
        ) items) in
      Printf.sprintf "%scase (%s)\n%s\n%sendcase" ind expr_str items_str ind

  | While { condition; stmts; incs; _ } ->
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
          Printf.sprintf "%s[%s +: %s]" base
            (generate_sv_with_interfaces l 0 [] |> String.trim)
            (generate_sv_with_interfaces w 0 [] |> String.trim)
      | Some l, None ->
          Printf.sprintf "%s[%s]" base (generate_sv_with_interfaces l 0 [] |> String.trim)
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

  | Initial { suspend; stmts; _ } ->
      let stmt_str = String.concat "\n" 
        (List.map (generate_sv_with_interfaces_indent (indent + 1) []) stmts) in
      Printf.sprintf "%sinitial begin\n%s\n%send" ind stmt_str ind

  | InitialStatic { suspend; process; stmts; _ } ->
      let stmt_str = String.concat "\n"
        (List.map (generate_sv_with_interfaces_indent (indent + 1) []) stmts) in
      Printf.sprintf "%sinitial begin\n%s\n%send" ind stmt_str ind

  | Final { suspend; process; stmts; _ } ->
      let stmt_str = String.concat "\n"
        (List.map (generate_sv_with_interfaces_indent (indent + 1) []) stmts) in
      Printf.sprintf "%sfinal begin\n%s\n%send" ind stmt_str ind

  | EventCtrl { sense; stmts; _ } ->
      let sense_str = String.concat "" 
        (List.map (generate_sv_with_interfaces_indent 0 []) sense) in
      let stmt_str = match stmts with
        | [] -> ""
        | _ -> "\n" ^ String.concat "\n" (List.map (generate_sv_with_interfaces_indent indent []) stmts)
      in
      Printf.sprintf "%s%s%s" ind sense_str stmt_str

  | Delay { cycle; lhs; stmts; _ } ->
      let delay_str = generate_sv_with_interfaces lhs 0 [] |> String.trim in
      let stmt_str = match stmts with
        | [stmt] -> " " ^ (generate_sv_with_interfaces stmt 0 [] |> String.trim)
        | _ -> " begin\n" ^ String.concat "\n" (List.map (generate_sv_with_interfaces_indent (indent + 1) []) stmts) ^ "\n" ^ ind ^ "end"
      in
      Printf.sprintf "%s#%s%s" ind delay_str stmt_str

  | Package { name; stmts; _ } ->
      let internal_str = String.concat "\n\n" 
        (List.map (generate_sv_with_interfaces_indent 1 []) stmts) in
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
        | Some (UnionType { packed; members; _ }) ->
            let pack_str = if packed then "packed " else "" in
            let members_str = extract_struct_members (Some (StructType { name = ""; packed; members })) in
            if List.length members_str > 0 then
              Printf.sprintf "typedef %sunion {\n%s\n    } %s;" pack_str 
                (String.concat "\n" members_str) name
            else
              Printf.sprintf "typedef %sunion {\n        // members\n    } %s;" pack_str name
        | Some typ -> Printf.sprintf "typedef %s %s;" (resolve_type (Some typ)) name
        | None -> Printf.sprintf "typedef ... %s;" name
      in
      Printf.sprintf "%s%s" ind type_str

  | Func { name; dtype_ref; stmts; vars; _ } ->
      let return_type = resolve_type dtype_ref in

      let inputs = List.filter (function
        | Var { var_type = "PORT"; direction = "INPUT"; _ } -> true
        | _ -> false
      ) vars in

      let params_str = 
        if List.length inputs > 0 then
          "\n" ^
          (String.concat "\n" (List.map (fun v ->
            match v with
            | Var { name = vname; dtype_ref = vtype; dtype_name; _ } ->
                let vtype_str = resolve_type_with_brackets dtype_name vtype in
                Printf.sprintf "        input %s %s;" vtype_str vname
            | _ -> ""
          ) inputs)) ^
          "\n"
        else
          ""
      in

      let body_str = String.concat "\n" 
        (List.map (generate_sv_with_interfaces_indent (indent + 2) []) stmts) in

      Printf.sprintf "%sfunction automatic %s %s();%s%s\n%sendfunction" 
        ind return_type name params_str body_str ind

  | Task { name; dtype_ref; stmts; vars; _ } ->
      let inputs = List.filter (function
        | Var { var_type = "PORT"; _ } -> true
        | _ -> false
      ) vars in

      let params_str = 
        if List.length inputs > 0 then
          "\n" ^
          (String.concat "\n" (List.map (fun v ->
            match v with
            | Var { name = vname; dtype_ref = vtype; dtype_name; direction; _ } ->
                let vtype_str = resolve_type_with_brackets dtype_name vtype in
                let dir_str = String.lowercase_ascii direction in
                Printf.sprintf "        %s %s %s;" dir_str vtype_str vname
            | _ -> ""
          ) inputs)) ^
          "\n"
        else
          ""
      in

      let body_str = String.concat "\n"
        (List.map (generate_sv_with_interfaces_indent (indent + 2) []) stmts) in

      Printf.sprintf "%stask automatic %s();%s%s\n%sendtask"
      ind name params_str body_str ind
| Display { fmt; file; _ } ->
      ""

  | Sformatp { expr; scope; _ } ->
      ""

  | Text { text; _ } -> ""

  | Time { dtype; _ } -> "$time"

  | StmtExpr { expr; _ } ->
      generate_sv_with_interfaces expr indent []

  | Cexpr { dtype; expr; _ } ->
      ""

  | Sampled { dtype; expr; _ } ->
      (match expr with
      | e :: _ -> generate_sv_with_interfaces e 0 []
      | [] -> "")

  | ScopeName { dtype; _ } ->
      ""

  | ConsPack { dtype_ref; members; _ } ->
      let members_str = String.concat ", "
        (List.map (generate_sv_with_interfaces_indent 0 []) members) in
      Printf.sprintf "'{%s}" members_str

  | ConsPackMember { dtype_ref; rhs; _ } ->
      generate_sv_with_interfaces rhs 0 []

  | Replicate { dtype_ref; src; count; _ } ->
      Printf.sprintf "{%s{%s}}"
        (generate_sv_with_interfaces count 0 [] |> String.trim)
        (generate_sv_with_interfaces src 0 [] |> String.trim)

  | InsideRange { lhs; rhs; _ } ->
      Printf.sprintf "%s inside {%s}"
        (generate_sv_with_interfaces lhs 0 [] |> String.trim)
        (generate_sv_with_interfaces rhs 0 [] |> String.trim)

  | Stop { fatal; _ } ->
      if fatal then
        Printf.sprintf "%s$fatal;" ind
      else
        Printf.sprintf "%s$finish;" ind

  | JumpBlock { stmt; _ } ->
      String.concat "\n" (List.map (generate_sv_with_interfaces_indent indent []) stmt)

  | JumpGo { label; _ } ->
      Printf.sprintf "%s// Jump to label" ind

  | InitArray { inits; _ } ->
      let init_str = String.concat ", "
        (List.map (generate_sv_with_interfaces_indent 0 []) inits) in
      Printf.sprintf "'{%s}" init_str

  | InitItem { value; _ } ->
      let val_str = String.concat ", "
        (List.map (generate_sv_with_interfaces_indent 0 []) value) in
      val_str

  | Fopen { dtype_ref; filename; mode; _ } ->
      let fname = String.concat "" 
        (List.map (generate_sv_with_interfaces_indent 0 []) filename) in
      let mode_str = String.concat "" 
        (List.map (generate_sv_with_interfaces_indent 0 []) mode) in
      Printf.sprintf "$fopen(%s, %s)" fname mode_str

  | Fclose { file; _ } ->
      let file_str = String.concat "" 
        (List.map (generate_sv_with_interfaces_indent 0 []) file) in
      Printf.sprintf "%s$fclose(%s);" ind file_str

  | Itord { dtype_ref; lhs; _ } ->
      let lhs_str = String.concat "" 
        (List.map (generate_sv_with_interfaces_indent 0 []) lhs) in
      lhs_str

  | CvtPackString { dtype_ref; lhs; _ } ->
      let lhs_str = String.concat "" 
        (List.map (generate_sv_with_interfaces_indent 0 []) lhs) in
      Printf.sprintf "$sformatf(%s)" lhs_str

  | ValuePlusArgs { dtype_ref; search; out; _ } ->
      let search_str = String.concat "" 
        (List.map (generate_sv_with_interfaces_indent 0 []) search) in
      let out_str = String.concat "" 
        (List.map (generate_sv_with_interfaces_indent 0 []) out) in
      Printf.sprintf "$value$plusargs(%s, %s)" search_str out_str

  | TestPlusArgs { dtype_ref; search; _ } ->
      let search_str = String.concat "" 
        (List.map (generate_sv_with_interfaces_indent 0 []) search) in
      Printf.sprintf "$test$plusargs(%s)" search_str

  | CMethodHard { dtype_ref; from; pins; _ } ->
      let from_str = generate_sv_with_interfaces from 0 [] |> String.trim in
      let pins_str = String.concat ", "
        (List.map (generate_sv_with_interfaces_indent 0 []) pins) in
      Printf.sprintf "%s.method(%s)" from_str pins_str

  | CaseItem { conditions; stmts; _ } ->
      let cond_str = String.concat ", "
        (List.map (fun c -> generate_sv_with_interfaces c 0 [] |> String.trim) conditions) in
      let stmt_str = String.concat "\n"
        (List.map (generate_sv_with_interfaces_indent indent []) stmts) in
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
  
  (* Separate statements, keeping generate blocks together *)
  let (regular_ports, cells, gen_blocks, other_stmts) = 
    List.fold_left (fun (ports, cells, gens, others) stmt ->
      match stmt with
      | Var { var_type = "PORT"; _ } as v -> (v :: ports, cells, gens, others)
      | Var { var_type = "IFACEREF"; _ } -> (ports, cells, gens, others)
      | Cell _ as c -> (ports, c :: cells, gens, others)
      | Always { always; senses; stmts = astmts; _ } as a ->
          (* Filter out assertion always blocks *)
          let has_display = List.exists (function
            | Display _ -> true
            | If { condition = Cexpr _; _ } -> true
            | _ -> false
          ) astmts in
          if has_display then (ports, cells, gens, others)
          else (ports, cells, gens, a :: others)
      | Begin { name = bname; is_generate = true; _ } as b ->
          (* Keep all generate blocks together *)
          (ports, cells, b :: gens, others)
      | _ -> (ports, cells, gens, stmt :: others)
    ) ([], [], [], []) stmts in

  let port_decls = List.map (fun v -> generate_sv_with_interfaces v 1 interfaces) regular_ports in
  
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
                Printf.sprintf "  %s %s ();" module_name name)
        | Some (Interface _) -> ""
        | _ -> "  // Unknown cell")
    | _ -> ""
  ) cells in

  (* Process generate blocks - keep parent and children together *)
  let gen_stmts = List.map (generate_sv_with_interfaces_indent 1 interfaces) (List.rev gen_blocks) in

  let other_assigns = List.map (fun stmt ->
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
        generate_sv_with_interfaces (AssignW { lhs = fixed_lhs; rhs = fixed_rhs }) 1 interfaces
    | _ -> generate_sv_with_interfaces stmt 1 interfaces
  ) other_stmts in

  let port_str = String.concat ",\n" port_decls in
  let body_parts = signal_decls @ cell_stmts @ gen_stmts @ other_assigns in
  let body = String.concat "\n" (List.filter (fun s -> s <> "") body_parts) in

  Printf.sprintf "module %s (\n%s\n);\n%s\nendmodule" name port_str body

and generate_interface_module_with_interfaces name stmts interface_refs interfaces =
  match interface_refs with
  | (var_name, iface_name, modport_name, modportp) :: _ ->
      let interface_def = find_interface_by_name iface_name interfaces in
      let flattened_ports = generate_interface_ports var_name modportp interface_def in
      let port_str = String.concat ",\n" flattened_ports in
      
      let module_port_names = get_modport_directions modportp |> List.map fst in
      
      let internals = List.filter (function
        | Var { var_type = "IFACEREF"; _ } -> false
        | _ -> true
      ) stmts in
      
      let fixed_internals = List.map (fix_varxref_in_node module_port_names var_name) internals in
      let internal_str = String.concat "\n" (List.map (generate_sv_with_interfaces_indent 1 interfaces) fixed_internals) in
      
      Printf.sprintf "module %s (\n%s\n);\n%s\nendmodule" name port_str internal_str
  | [] -> generate_regular_module name stmts []

and generate_regular_module name stmts top_level_ports =
  let ports = List.filter_map (function
    | Var { var_type = "PORT"; _ } as v -> Some (generate_sv v 1 |> String.trim)
    | _ -> None
  ) stmts in
  
  let internals = List.filter (function
    | Var { var_type = "PORT"; _ } -> false
    | _ -> true
  ) stmts in
  
  let port_str = if ports = [] then "" else Printf.sprintf " (\n  %s\n)" (String.concat ",\n  " ports) in
  let internal_str = String.concat "\n" (List.map (generate_sv_indent 1) internals) in
  
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
  match pins with
  | [Pin { expr = Some (VarRef { name = "clk"; _ }); _ }] -> [".clk(clk)"]
  | [Pin { expr = Some (VarRef { name; _ }); _ }] -> [Printf.sprintf ".sif(%s)" name]  
  | _ -> []
