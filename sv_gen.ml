open Sv_ast

let debug = ref true

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
    (* Find the variable definition in the interface *)
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
  (* For assignments, we need to be more careful about when to use direct port names *)
  match var_name with
  | name when List.mem name top_level_ports -> 
      (* This is a top-level port - use direct name for LHS of assignments *)
      name
  | _ -> 
      (* Use flattened interface signal name *)
      Printf.sprintf "%s_%s" dotted_name var_name

(* Generate port connections for module instantiation *)
let generate_port_connections iface_var_name modport_def interface_def top_level_ports =
  if !debug then Printf.printf "DEBUG: generate_port_connections called with iface_var_name=%s\n" iface_var_name;
  let directions = get_modport_directions modport_def in
  if !debug then Printf.printf "DEBUG: Found %d directions from modport\n" (List.length directions);
  List.iter (fun (name, dir) -> if !debug then Printf.printf "DEBUG: Direction %s -> %s\n" name dir) directions;
  
  (* Always include clock connection *)
  let clock_conn = ".clk(clk)" in
  
  let other_conns = List.fold_left (fun acc (var_name, _) ->
    if var_name = "clk" then acc  (* Skip clk, already handled *)
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

let rec generate_sv node indent =
  let ind = String.make (indent * 2) ' ' in
  match node with
  | Netlist modules ->
      (* Extract interfaces from top level for passing to modules *)
      let interfaces = List.filter_map (function
        | Interface _ as iface -> Some iface
        | _ -> None
      ) modules in
      String.concat "\n\n" (List.map (generate_sv_with_interfaces_indent 0 interfaces) modules)

  | Module { name; stmts; _ } ->
      Printf.printf "ERROR: generate_sv called on Module without interface context\n";
      generate_regular_module name stmts []

  | Interface { name; _ } ->
      ""  (* Skip interfaces in output *)

  | Cell { name; modp_addr; pins; _ } ->
      (match modp_addr with
      | Some (Interface { name = iface_name; _ }) ->
          ""  (* Skip interface instantiations *)
      | Some (Module { name = module_name; _ } as mod_def) ->
          let connections = generate_cell_connections pins (Some mod_def) [] in
          Printf.sprintf "%s%s %s (%s);" ind module_name name (String.concat ", " connections)
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
        | Some v -> " = " ^ (generate_sv_with_interfaces v 0 [] |> String.trim)
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
      Printf.sprintf "%s_%s" dotted name

  | SenTree senses ->
      Printf.sprintf "@(%s)" 
        (String.concat " or " (List.map (generate_sv_with_interfaces_indent 0 []) senses))

  | SenItem { edge_str; signal; _ } ->
      Printf.sprintf "%s %s" edge_str (generate_sv_with_interfaces signal 0 [] |> String.trim)

  | BinaryOp { op; lhs; rhs; _ } ->
      let op_str = match op with
        | "ADD" -> "+" | "SUB" -> "-" | "MUL" -> "*" | "DIV" -> "/"
        | "AND" -> "&" | "OR" -> "|" | "XOR" -> "^"
        | "EQ" -> "==" | "NEQ" -> "!=" | "LT" -> "<" | "GT" -> ">"
        | _ -> op
      in
      Printf.sprintf "(%s %s %s)" 
        (generate_sv_with_interfaces lhs 0 [] |> String.trim) op_str (generate_sv_with_interfaces rhs 0 [] |> String.trim)

  | Const { name; _ } -> name

  | _ -> "/* Unhandled node */"

and generate_sv_indent indent node = generate_sv node indent

(* New function that carries interface context *)
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

(* Generate top module with interface flattening *)
and generate_top_module_with_interfaces name stmts interface_refs interfaces =
  if !debug then Printf.printf "DEBUG: Generating top module '%s' with %d interface refs\n" name (List.length interface_refs);
  
  (* Extract top level port names *)
  let top_level_ports = List.fold_left (fun acc stmt ->
    match stmt with
    | Var { var_type = "PORT"; name; _ } -> name :: acc
    | _ -> acc
  ) [] stmts in
  
  let (regular_ports, cells, other_stmts) = 
    List.fold_left (fun (ports, cells, others) stmt ->
      match stmt with
      | Var { var_type = "PORT"; _ } as v -> (v :: ports, cells, others)
      | Var { var_type = "IFACEREF"; _ } -> (ports, cells, others)
      | Cell _ as c -> (ports, c :: cells, others)
      | _ -> (ports, cells, stmt :: others)
    ) ([], [], []) stmts in

  (* Generate regular port declarations *)
  let port_decls = List.map (fun v -> generate_sv_with_interfaces v 1 interfaces) regular_ports in
  
  (* Generate interface signal declarations *)
  let signal_decls = List.fold_left (fun acc (var_name, iface_name, _, _) ->
    let interface_def = find_interface_by_name iface_name interfaces in
    let signals = generate_interface_signals var_name interface_def in
    signals @ acc
  ) [] interface_refs in

  (* Generate cell instantiations with proper connections *)
  let cell_stmts = List.map (fun cell ->
    match cell with
    | Cell { name; modp_addr; pins; _ } ->
        (match modp_addr with
        | Some (Module { name = module_name; stmts = mod_stmts; _ }) ->
            let mod_interface_refs = extract_interface_refs mod_stmts in
            (match mod_interface_refs with
            | (_, iface_name, _, modportp) :: _ ->
                let interface_def = find_interface_by_name iface_name interfaces in
                (* Extract the actual interface variable name from our interface_refs *)
                let iface_var_name = match interface_refs with
                  | (var_name, _, _, _) :: _ -> var_name
                  | [] -> "sif"  (* fallback *)
                in
                let connections = generate_port_connections iface_var_name modportp interface_def top_level_ports in
                Printf.sprintf "  %s %s (%s);" module_name name (String.concat ", " connections)
            | [] ->
                Printf.sprintf "  %s %s ();" module_name name)
        | Some (Interface _) -> ""  (* Skip interface instantiations *)
        | _ -> "  // Unknown cell")
    | _ -> ""
  ) cells in

  (* Generate other assignments, fixing VarXRef nodes *)
  let other_assigns = List.map (fun stmt ->
    match stmt with
    | AssignW { lhs; rhs } ->
        (* For assign statements, we need special handling *)
        let fixed_rhs = match rhs with
          | VarXRef { name; dotted; _ } -> 
              let signal_name = Printf.sprintf "%s_%s" dotted name in
              VarRef { name = signal_name; access = "RD" }
          | _ -> rhs
        in
        let fixed_lhs = match lhs with
          | VarRef { name; _ } when List.mem name top_level_ports -> lhs  (* Keep top-level port as-is *)
          | _ -> fix_varxref_in_node top_level_ports "" lhs
        in
        generate_sv_with_interfaces (AssignW { lhs = fixed_lhs; rhs = fixed_rhs }) 1 interfaces
    | _ -> generate_sv_with_interfaces stmt 1 interfaces
  ) other_stmts in

  let port_str = String.concat ",\n" port_decls in
  let body_parts = signal_decls @ cell_stmts @ other_assigns in
  let body = String.concat "\n" (List.filter (fun s -> s <> "") body_parts) in

  Printf.sprintf "module %s (\n%s\n);\n%s\nendmodule" name port_str body

(* Generate interface module with flattened ports *)
and generate_interface_module_with_interfaces name stmts interface_refs interfaces =
  match interface_refs with
  | (var_name, iface_name, modport_name, modportp) :: _ ->
      let interface_def = find_interface_by_name iface_name interfaces in
      let flattened_ports = generate_interface_ports var_name modportp interface_def in
      let port_str = String.concat ",\n" flattened_ports in
      
      (* Extract top level port names for this module *)
      let module_port_names = get_modport_directions modportp |> List.map fst in
      
      (* Filter out interface references from internals and fix VarXRef *)
      let internals = List.filter (function
        | Var { var_type = "IFACEREF"; _ } -> false
        | _ -> true
      ) stmts in
      
      let fixed_internals = List.map (fix_varxref_in_node module_port_names var_name) internals in
      let internal_str = String.concat "\n" (List.map (generate_sv_with_interfaces_indent 1 interfaces) fixed_internals) in
      
      Printf.sprintf "module %s (\n%s\n);\n%s\nendmodule" name port_str internal_str
  | [] -> generate_regular_module name stmts []

(* Generate regular module *)
and generate_regular_module name stmts top_level_ports =
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

(* Find interface definition by name in the interface list *)
and find_interface_by_name iface_name interfaces =
  List.find_opt (function
    | Interface { name; _ } when name = iface_name -> true
    | _ -> false
  ) interfaces

(* Extract interface variable name from pins *)
and extract_iface_var_name_from_pins pins =
  match pins with
  | Pin { expr = VarRef { name; _ }; _ } :: _ when name <> "clk" -> name
  | _ -> "sif"  (* default fallback *)

(* Generate cell connections based on pin information *)
and generate_cell_connections pins module_def top_level_ports =
  match pins with
  | [Pin { expr = VarRef { name = "clk"; _ }; _ }] -> [".clk(clk)"]
  | [Pin { expr = VarRef { name; _ }; _ }] -> [Printf.sprintf ".sif(%s)" name]  
  | _ -> []
  
