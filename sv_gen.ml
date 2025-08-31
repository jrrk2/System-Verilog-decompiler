open Sv_ast

let resolve_type = function
  | Some (RefType { name; _ }) -> name
  | Some (BasicType { keyword; range = Some r; _ }) -> keyword ^ " [" ^ r ^ "]"
  | Some (BasicType { keyword; range = None; _ }) -> keyword
  | Some (EnumType { name; _ }) -> name
  | _ -> "logic"

let resolve_type_with_brackets dtype_name dtype_ref =
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
  List.fold_left (fun acc stmt ->
    match stmt with
    | Var { var_type = "IFACEREF"; dtype_ref = Some (IntfRefType { ifacename; modportname; modportp; _ }); name; _ } ->
        (name, ifacename, modportname, modportp) :: acc
    | _ -> acc
  ) [] stmts

(* Get modport directions from a modport definition *)
let get_modport_directions = function
  | Some (Modport { vars; _ }) -> 
      List.fold_left (fun acc var ->
        match var with
        | ModportVarRef { name; direction; _ } -> (name, direction) :: acc
        | _ -> acc
      ) [] vars
  | Some _ -> []
  | None -> []

(* Generate flattened ports for interface module *)
let generate_interface_ports iface_name modport_def interface_def =
  let directions = get_modport_directions modport_def in
  let interface_vars = match interface_def with
    | Some (Interface { stmts; _ }) -> stmts
    | _ -> []
  in
  
  List.fold_left (fun acc (var_name, direction) ->
    (* Find the variable definition in the interface *)
    let var_info = List.find_opt (function
      | Var { name; _ } when name = var_name -> true
      | _ -> false
    ) interface_vars in
    
    match var_info with
    | Some (Var { dtype_ref; dtype_name; _ }) ->
        let var_type = resolve_type_with_brackets dtype_name dtype_ref in
        let port_dir = String.lowercase_ascii direction in
        (Printf.sprintf "  %s %s %s" port_dir var_type var_name) :: acc
    | _ -> acc
  ) [] directions |> List.rev

(* Generate flattened signal declarations for top module *)
let generate_interface_signals iface_var_name interface_def =
  match interface_def with
  | Some (Interface { stmts; _ }) ->
      List.fold_left (fun acc stmt ->
        match stmt with
        | Var { name; dtype_ref; dtype_name; var_type = "VAR"; _ }
        | Var { name; dtype_ref; dtype_name; var_type = "PORT"; direction = "INPUT"; _ } ->
            let signal_type = resolve_type_with_brackets dtype_name dtype_ref in
            let flattened_name = Printf.sprintf "%s_%s" iface_var_name name in
            (Printf.sprintf "  %s %s;" signal_type flattened_name) :: acc
        | _ -> acc
      ) [] stmts |> List.rev
  | _ -> []

(* Generate port connections for module instantiation *)
let generate_port_connections iface_var_name modport_def =
  let directions = get_modport_directions modport_def in
  List.map (fun (var_name, _) ->
    let flattened_name = Printf.sprintf "%s_%s" iface_var_name var_name in
    Printf.sprintf ".%s(%s)" var_name flattened_name
  ) directions

let rec generate_sv node indent =
  let ind = String.make (indent * 2) ' ' in
  match node with
  | Netlist modules ->
      String.concat "\n\n" (List.map (generate_sv_indent 0) modules)

  | Module { name; stmts; _ } ->
      let interface_refs = extract_interface_refs stmts in
      let has_regular_ports = List.exists (function | Var { var_type = "PORT"; _ } -> true | _ -> false) stmts in
      
      if has_regular_ports then
        generate_top_module name stmts interface_refs
      else if interface_refs <> [] then
        generate_interface_module name stmts interface_refs
      else
        generate_regular_module name stmts

  | Interface { name; _ } ->
      Printf.sprintf "// Interface %s flattened" name

  | Cell { name; modp_addr; pins; _ } ->
      (match modp_addr with
      | Some (Interface { name = iface_name; _ }) ->
          Printf.sprintf "%s// Interface %s instantiation flattened away" ind iface_name
      | Some (Module { name = module_name; _ } as mod_def) ->
          let connections = generate_cell_connections_for_module pins (Some mod_def) in
          Printf.sprintf "%s%s %s(%s);" ind module_name name (String.concat ", " connections)
      | _ -> Printf.sprintf "%s// Unknown cell %s" ind name)

  | Var { name; dtype_ref; var_type; direction; value; dtype_name; is_param; _ } ->
      let resolved_type = resolve_type_with_brackets dtype_name dtype_ref in
      let prefix = match var_type with
        | "LPARAM" -> "localparam"
        | "GPARAM" -> "parameter"
        | "PORT" -> String.lowercase_ascii direction
        | "GENVAR" -> "genvar"
        | _ -> if is_param then "parameter" else ""
      in
      let val_str = match value with
        | Some v -> " = " ^ (generate_sv v 0 |> String.trim)
        | None -> ""
      in
      if var_type = "PORT" then
        Printf.sprintf "%s %s %s%s" prefix resolved_type name val_str
      else if prefix <> "" then
        Printf.sprintf "%s%s %s %s%s;" ind prefix resolved_type name val_str
      else
        Printf.sprintf "%s%s %s%s;" ind resolved_type name val_str

  | Always { always; senses; stmts; _ } ->
      let sense_str = match senses with
        | [] -> ""
        | _ -> " " ^ (String.concat "" (List.map (generate_sv_indent 0) senses))
      in
      let stmt_str = String.concat "\n" (List.map (generate_sv_indent (indent + 1)) stmts) in
      Printf.sprintf "%s%s%s begin\n%s\n%send" ind always sense_str stmt_str ind

  | Assign { lhs; rhs; is_blocking; _ } ->
      let op = if is_blocking then "=" else "<=" in
      Printf.sprintf "%s%s %s %s;" ind 
        (generate_sv lhs 0 |> String.trim) op (generate_sv rhs 0 |> String.trim)

  | AssignW { lhs; rhs; _ } ->
      Printf.sprintf "%sassign %s = %s;" ind 
        (generate_sv lhs 0 |> String.trim) (generate_sv rhs 0 |> String.trim)

  | VarRef { name; _ } -> name

  | VarXRef { name; dotted; _ } ->
      Printf.sprintf "%s_%s" dotted name

  | SenTree senses ->
      Printf.sprintf "@(%s)" 
        (String.concat " or " (List.map (generate_sv_indent 0) senses))

  | SenItem { edge_str; signal; _ } ->
      Printf.sprintf "%s %s" edge_str (generate_sv signal 0 |> String.trim)

  | BinaryOp { op; lhs; rhs; _ } ->
      let op_str = match op with
        | "ADD" -> "+" | "SUB" -> "-" | "MUL" -> "*" | "DIV" -> "/"
        | "AND" -> "&" | "OR" -> "|" | "XOR" -> "^"
        | "EQ" -> "==" | "NEQ" -> "!=" | "LT" -> "<" | "GT" -> ">"
        | _ -> op
      in
      Printf.sprintf "(%s %s %s)" 
        (generate_sv lhs 0 |> String.trim) op_str (generate_sv rhs 0 |> String.trim)

  | Const { name; _ } -> name

  | _ -> "/* Unhandled node */"

and generate_sv_indent indent node = generate_sv node indent

(* Generate top module with interface flattening *)
and generate_top_module name stmts interface_refs =
  let (regular_ports, cells, other_stmts) = 
    List.fold_left (fun (ports, cells, others) stmt ->
      match stmt with
      | Var { var_type = "PORT"; _ } as v -> (v :: ports, cells, others)
      | Var { var_type = "IFACEREF"; _ } -> (ports, cells, others)
      | Cell _ as c -> (ports, c :: cells, others)
      | _ -> (ports, cells, stmt :: others)
    ) ([], [], []) stmts in

  (* Generate regular port declarations *)
  let port_decls = List.map (fun v -> generate_sv v 1) regular_ports in
  
  (* Generate interface signal declarations *)
  let signal_decls = List.fold_left (fun acc (var_name, iface_name, _, modportp) ->
    let interface_def = find_interface_in_stmts iface_name stmts in
    let signals = generate_interface_signals var_name interface_def in
    signals @ acc
  ) [] interface_refs in

  (* Generate clock assignments *)
  let clock_assigns = List.map (fun (var_name, _, _, _) ->
    Printf.sprintf "  assign %s_clk = clk;" var_name
  ) interface_refs in

  (* Generate cell instantiations *)
  let cell_stmts = List.map (generate_sv_indent 1) cells in

  (* Generate other assignments *)
  let other_assigns = List.map (generate_sv_indent 1) other_stmts in

  let port_str = String.concat ",\n" port_decls in
  let body_parts = signal_decls @ clock_assigns @ cell_stmts @ other_assigns in
  let body = String.concat "\n" body_parts in

  Printf.sprintf "module %s (\n%s\n);\n%s\nendmodule" name port_str body

(* Generate interface module with flattened ports *)
and generate_interface_module name stmts interface_refs =
  match interface_refs with
  | (var_name, iface_name, modport_name, modportp) :: _ ->
      let interface_def = find_interface_in_stmts iface_name stmts in
      let flattened_ports = generate_interface_ports var_name modportp interface_def in
      let port_str = String.concat ",\n" flattened_ports in
      
      (* Filter out interface references from internals *)
      let internals = List.filter (function
        | Var { var_type = "IFACEREF"; _ } -> false
        | _ -> true
      ) stmts in
      
      (* Fix VarXRef nodes to use direct variable names *)
      let fixed_internals = List.map fix_varxref_stmt internals in
      let internal_str = String.concat "\n" (List.map (generate_sv_indent 1) fixed_internals) in
      
      Printf.sprintf "module %s (\n%s\n);\n%s\nendmodule" name port_str internal_str
  | [] -> generate_regular_module name stmts

(* Generate regular module *)
and generate_regular_module name stmts =
  let ports = List.filter_map (function
    | Var { var_type = "PORT"; _ } as v -> Some (generate_sv v 1 |> String.trim)
    | _ -> None
  ) stmts in
  
  let internals = List.filter (function
    | Var { var_type = "PORT"; _ } -> false
    | _ -> true
  ) stmts in
  
  let port_str = if ports = [] then "" else Printf.sprintf " (\n%s\n)" (String.concat ",\n  " ports) in
  let internal_str = String.concat "\n" (List.map (generate_sv_indent 1) internals) in
  
  Printf.sprintf "module %s%s;\n%s\nendmodule" name port_str internal_str

(* Fix VarXRef statements to use direct variable names *)
and fix_varxref_stmt = function
  | Always { always; senses; stmts } ->
      let fixed_stmts = List.map (fun stmt ->
        let rec fix_node = function
          | VarXRef { name; _ } -> VarRef { name; access = "RD" }
          | Assign { lhs; rhs; is_blocking } ->
              Assign { lhs = fix_node lhs; rhs = fix_node rhs; is_blocking }
          | BinaryOp { op; lhs; rhs } ->
              BinaryOp { op; lhs = fix_node lhs; rhs = fix_node rhs }
          | node -> node
        in fix_node stmt
      ) stmts in
      Always { always; senses; stmts = fixed_stmts }
  | stmt -> stmt

(* Find interface definition in statements *)
and find_interface_in_stmts iface_name stmts =
  List.find_opt (function
    | Interface { name; _ } when name = iface_name -> true
    | _ -> false
  ) stmts

(* Generate cell connections based on module's interface references *)
and generate_cell_connections_for_module pins module_stmts =
  (* Extract interface info from the target module *)
  let module_interface_refs = match module_stmts with
    | Some (Module { stmts; _ }) -> extract_interface_refs stmts
    | _ -> []
  in
  
  List.fold_left (fun acc pin ->
    match pin with
    | Pin { expr = VarRef { name = "clk"; _ }; _ } -> 
        ".clk(clk)" :: acc
    | Pin { expr = VarRef { name = iface_name; _ }; _ } ->
        (* Generate flattened connections based on the module's interface requirements *)
        (match module_interface_refs with
        | (_, _, _, Some (Modport { vars; _ })) :: _ ->
            List.fold_left (fun conn_acc var ->
              match var with
              | ModportVarRef { name = var_name; _ } ->
                  let flattened_name = Printf.sprintf "%s_%s" iface_name var_name in
                  (Printf.sprintf ".%s(%s)" var_name flattened_name) :: conn_acc
              | _ -> conn_acc
            ) acc vars
        | _ -> acc)
    | _ -> acc
    ) [] pins |> List.rev
