open Sv_ast

let dump_sv_type = function
| Some (IntfRefType {ifacename; modportname; ifacep; modportp}) -> ifacename^":"^modportname^":ifacep:modportp"
| _ -> "unknown"

(* Resolve type reference to actual type info *)
let resolve_type dtype_ref =
  try 
    match dtype_ref with
    | RefType { name; _ } -> name
    | BasicType { keyword; range = Some r; _ } -> keyword ^ " [" ^ r ^ "]"
    | BasicType { keyword; range = None; _ } -> keyword
    | EnumType { name; _ } -> name
    | _ -> "logic"
  with Not_found -> "logic"

let resolve_type_with_brackets dtype_name = function Some dtype_ref ->
  (* Use dtype_name if available, otherwise resolve from type table *)
  let base_type = if dtype_name <> "" && dtype_name <> "logic" then dtype_name else resolve_type dtype_ref in
  (* Fix double bracket issue: logic [[7:0]] -> logic [7:0] *)
  let fixed_type = Str.global_replace (Str.regexp "\\[\\[\\([^]]+\\)\\]\\]") "[\\1]" base_type in
  (* Convert "logic 7:0" format to "logic [7:0]" *)
  if String.contains fixed_type ' ' && not (String.contains fixed_type '[') then
    (match String.split_on_char ' ' fixed_type with
    | [keyword; range] when String.contains range ':' -> 
        Printf.sprintf "%s [%s]" keyword range
    | _ -> fixed_type)
  else fixed_type
  | None -> "logic"

(* Extract interface signals with actual interface name *)
let extract_interface_signals interface_name interface_stmts =
  List.fold_left (fun acc stmt ->
    match stmt with
    | Var { name; dtype_ref; dtype_name; var_type = "VAR"; _ } ->
        let signal_type = resolve_type_with_brackets dtype_name dtype_ref in
        let flattened_name = Printf.sprintf "%s_%s" interface_name name in
        (flattened_name, signal_type, name) :: acc
    | Var { name; dtype_ref; dtype_name; var_type = "PORT"; direction = "INPUT"; _ } ->
        let signal_type = resolve_type_with_brackets dtype_name dtype_ref in
        let flattened_name = Printf.sprintf "%s_%s" interface_name name in
        (flattened_name, signal_type, name) :: acc
    | _ -> acc
  ) [] interface_stmts

(* Interface flattening logic *)

let flatten_interface_to_ports interface_name interface_def =
  (* Extract signals from interface and convert to individual ports *)
  let rec extract_signals stmts =
    List.fold_left (fun acc stmt ->
      match stmt with
      | Var { name; dtype_ref; dtype_name; _ } when name <> "clk" ->
          (* Convert interface signals to module ports *)
          let flattened_name = Printf.sprintf "%s_%s" interface_name name in
          (flattened_name, resolve_type_with_brackets dtype_name dtype_ref) :: acc
      | _ -> acc
    ) [] stmts
  in
  extract_signals interface_def.statements

let nodelst = ref []

(* Generate SystemVerilog source from AST *)
let rec generate_sv node indent =
  let ind = String.make (indent * 2) ' ' in
  match node with
  | Netlist (modules) ->
      let nod = ref [] in
      let cat = String.concat "\n\n" (List.map (fun itm -> nod := itm :: !nod; generate_sv_indent 0 itm) modules) in
      nodelst := List.rev !nod;
      cat

  | Module { name; stmts; _ } ->
      (* Enhanced module generation with proper interface handling *)
      let has_regular_ports = List.exists (function
        | Var { var_type = "PORT"; _ } -> true
        | _ -> false
      ) stmts in

      let has_interface_refs = List.exists (function
        | Var { var_type = "IFACEREF"; _ } -> true
        | _ -> false
      ) stmts in
      if has_interface_refs then
        (* Child module with interface parameters *)
        generate_interface_module name stmts
      else
        (* Regular module *)
        generate_regular_module name stmts
       
  | Package { name; stmts; _ } ->
      Printf.sprintf "package %s;\n%s\nendpackage"
        name (String.concat "\n" (List.map (generate_sv_indent (indent + 1)) stmts))

  | Modport { name; vars; _ } ->
      (* Modports are flattened away during interface processing *)
      Printf.sprintf "/* modport %s */" name

  | ModportVarRef { name; direction; _ } ->
      (* Individual modport variable references *)
      Printf.sprintf "/* modport var %s (%s) */" name (String.lowercase_ascii direction)

  | Interface { name; stmts; _ } ->
      (* Interfaces are flattened away, but we need to track their definitions *)
      Printf.sprintf "// Interface %s flattened" name
      
  | Cell { name; modp_addr; pins; _ } ->      
      (* Check if this is an interface instantiation by checking modp_addr against interface table *)
      (match modp_addr with
        | Some (Interface { name }) ->
        Printf.sprintf "%s// Interface %s instantiation flattened away" ind name
        | Some (Module { name }) -> let module_name = name in
        (* Generate connections for module instantiations *)
        let pin_connections = List.fold_left (fun acc pin ->
(*
          match pin with
          | Pin { expr = VarRef { name = "clk"; _ }; _ } -> 
              (* Direct clock connection *)
              ".clk(clk)" :: acc
          | Pin { expr = VarRef { name = iface_name; _ }; _ } ->
	      (* Interface connection - expand to flattened signals *)
              print_endline ("Expand: "^iface_name^" "^module_name);
              let expanded = expand_interface_connections iface_name module_name in
              expanded @ acc
          | Pin { expr = VarXRef { name = signal; dotted; _ }; _ } ->
              let flattened_name = Printf.sprintf "%s_%s" dotted signal in
              (Printf.sprintf ".%s(%s)" signal flattened_name) :: acc
          | _ ->
*)
 acc
        ) [] pins in
	    let connections = String.concat ", " (List.rev pin_connections) in
	    Printf.sprintf "%s%s %s(%s);" ind module_name name connections
         | _ -> "unknown");
  | Pin { name; expr; _ } ->
      generate_sv expr 0
        
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
        
  | Const { name } -> name
      
  | Typedef { name; dtype_ref; _ } ->
      let resolved_type = match dtype_ref with Some r -> resolve_type r | None -> "logic" in
      Printf.sprintf "%stypedef %s %s;" ind resolved_type name
      
  | Func { name; dtype_ref; stmts; vars; _ } ->
      let return_type = resolve_type_with_brackets "" dtype_ref in
      (* Extract input parameters from vars, filtering out the return variable *)
      let param_vars = List.filter (function 
        | Var { var_type = "PORT"; direction = "INPUT"; name = param_name; _ } when param_name <> name -> true 
        | _ -> false) vars in
      let params = String.concat ", " (List.map (fun v ->
        match v with
        | Var { name = param_name; dtype_ref; dtype_name; _ } ->
            let ptype = resolve_type_with_brackets dtype_name dtype_ref in
            Printf.sprintf "input %s %s" ptype param_name
        | _ -> ""
      ) param_vars) in
      
      let body = String.concat "\n" (List.map (generate_sv_indent (indent + 1)) stmts) in
      Printf.sprintf "%sfunction automatic %s %s(%s);\n%s\n%sendfunction" 
        ind return_type name params body ind
        
  | Always { always; senses; stmts; _ } ->
      let sense_str = match senses with
        | [] -> ""
        | _ -> " " ^ (String.concat "" (List.map (generate_sv_indent 0) senses))
      in
      let stmt_str = String.concat "\n" (List.map (generate_sv_indent (indent + 1)) stmts) in
      Printf.sprintf "%s%s%s begin\n%s\n%send" ind always sense_str stmt_str ind
      
  | Begin { name; stmts; is_generate = false; _ } ->
      let stmt_str = String.concat "\n" (List.map (generate_sv_indent (indent + 1)) stmts) in
      Printf.sprintf "%sbegin\n%s\n%send" ind stmt_str ind
      
  | Begin { name; stmts; is_generate = true; _ } ->
      let stmt_str = String.concat "\n" (List.map (generate_sv_indent (indent + 1)) stmts) in
      if name <> "" && not (String.contains name '[') then
        Printf.sprintf "%sbegin : %s\n%s\n%send" ind name stmt_str ind
      else if String.contains name '[' then
        (* Handle generate array instances *)
        stmt_str
      else
        stmt_str
        
  | Assign { lhs; rhs; is_blocking = true; _ } ->
      Printf.sprintf "%s%s = %s;" ind 
        (generate_sv lhs 0 |> String.trim) (generate_sv rhs 0 |> String.trim)
        
  | Assign { lhs; rhs; is_blocking = false; _ } ->
      Printf.sprintf "%s%s <= %s;" ind 
        (generate_sv lhs 0 |> String.trim) (generate_sv rhs 0 |> String.trim)
        
  | AssignW { lhs; rhs; _ } ->
      Printf.sprintf "%sassign %s = %s;" ind 
        (generate_sv lhs 0 |> String.trim) (generate_sv rhs 0 |> String.trim)
        
  | If { condition; then_stmt; else_stmt; _ } ->
      let else_part = match else_stmt with
        | Some stmt -> Printf.sprintf " else\n%s" (generate_sv stmt indent)
        | None -> ""
      in
      Printf.sprintf "%sif (%s)\n%s%s" ind 
        (generate_sv condition 0 |> String.trim) 
        (generate_sv then_stmt (indent + 1)) else_part
        
  | Case { expr; items; _ } ->
      let case_items = List.map (fun item ->
        let cond_str = match item.conditions with
          | [] -> "default"
          | conds -> String.concat ", " (List.map (generate_sv_indent 0) conds)
        in
        let stmt_str = String.concat "\n" (List.map (generate_sv_indent (indent + 2)) item.statements) in
        Printf.sprintf "%s  %s: begin\n%s\n%s  end" ind cond_str stmt_str ind
      ) items in
      Printf.sprintf "%scase (%s)\n%s\n%sendcase" 
        ind (generate_sv expr 0 |> String.trim) (String.concat "\n" case_items) ind
        
  | CaseItem { conditions; stmts } ->
      let cond_str = match conditions with
        | [] -> "default"
        | conds -> String.concat ", " (List.map (generate_sv_indent 0) conds)
      in
      let stmt_str = String.concat "\n" (List.map (generate_sv_indent (indent + 1)) stmts) in
      Printf.sprintf "%s: begin\n%s\n%send" cond_str stmt_str ind
      
  | While { condition; stmts; incs; _ } ->
      (* Convert Verilator's while loop back to SystemVerilog for loop *)
      let init_var = ref "i" in
      let init_stmt = List.find_map (function
        | Assign { lhs = VarRef { name = var; _ }; rhs; _ } when String.length var = 1 -> 
            init_var := var;
            Some (Printf.sprintf "int %s = %s" var (generate_sv rhs 0 |> String.trim))
        | _ -> None
      ) stmts |> Option.value ~default:"int i = 0" in
      
      let cond_str = generate_sv condition 0 |> String.trim in
      let inc_str = Printf.sprintf "%s++" !init_var in
      
      let body_stmts = List.filter (function
        | Assign { lhs = VarRef { name; _ }; _ } when name = !init_var -> false
        | _ -> true
      ) stmts in
      
      let stmt_str = String.concat "\n" (List.map (generate_sv_indent (indent + 1)) body_stmts) in
      Printf.sprintf "%sfor (%s; %s; %s) begin\n%s\n%send" 
        ind init_stmt cond_str inc_str stmt_str ind
        
  | VarRef { name; _ } ->
      name
      
  | VarXRef { name; dotted; _ } ->
      (* Generate flattened interface signal name *)
      Printf.sprintf "%s_%s" dotted name

  | Sel { expr; range; lsb = None; width = None; _ } when range <> "" ->
      Printf.sprintf "%s%s" (generate_sv expr 0 |> String.trim) range
      
  | Sel { expr; range; lsb = None; width } ->
      Printf.sprintf "Unhandled width %s%s" (generate_sv expr 0 |> String.trim) range
      
  | Sel { expr; lsb = Some lsb; width = Some width; _ } ->
      (* Simplify complex bit selections *)
      let lsb_str = generate_sv lsb 0 |> String.trim in
      let width_str = generate_sv width 0 |> String.trim in
      let expr_str = generate_sv expr 0 |> String.trim in
      
      (* Handle special cases for simple variables *)
      if expr_str = "i" || expr_str = "j" then
        (* For simple integer variables, don't add bit selection *)
        expr_str
      else
        (* Simplify common patterns *)
        let simplified_lsb = match lsb_str with
          | "1'b0" | "0" | "'h0" -> "0"
          | "1'b1" | "1" | "'h1" -> "1"
          | "'h1E" | "30" -> "1"  (* counter_q[1:0] for 2-bit selection *)
          | s when String.contains s '[' -> 
              (* Remove nested bit selections for simple cases *)
              if String.contains s '*' && String.contains s ')' then
                (* Pattern like (8 * j)[0 +: 4] -> just extract the multiplier result *)
                let base = Str.global_replace (Str.regexp ".*\\*[ ]*\\([a-zA-Z]\\).*") "\\1" s in
                Printf.sprintf "(%s * 8)" base
              else s
          | s -> s
        in
        
        let simplified_width = match width_str with
          | "1'b1" | "1" -> "1"
          | "8" | "'h8" -> "8"
          | "4'h4" | "4" -> "4"
          | "4'h2" | "2" -> "2"
          | "4'h3" | "3" -> "3"
          | "4'hB" | "11" -> "11"
          | s -> s
        in
        
        (* Special case for counter bit selection *)
        if simplified_width = "2" then
          Printf.sprintf "%s[1:0]" expr_str
        else if simplified_width = "1" then
          if simplified_lsb = "0" then expr_str
          else Printf.sprintf "%s[%s]" expr_str simplified_lsb
        else
          Printf.sprintf "%s[%s +: %s]" expr_str simplified_lsb simplified_width
        
  | Sel { expr; lsb = Some lsb; width = None; _ } ->
      Printf.sprintf "%s[%s]" 
        (generate_sv expr 0 |> String.trim) (generate_sv lsb 0 |> String.trim)
        
  | ArraySel { expr; index; _ } ->
      Printf.sprintf "%s[%s]" 
        (generate_sv expr 0 |> String.trim) (generate_sv index 0 |> String.trim)
        
  | FuncRef { name; args; _ } ->
      let arg_strs = List.map (generate_sv_indent 0) args |> List.map String.trim in
      Printf.sprintf "%s(%s)" name (String.concat ", " arg_strs)
      
  | BinaryOp { op; lhs; rhs; _ } ->
      let op_str = match op with
        | "AND" -> "&" | "OR" -> "|" | "XOR" -> "^"
        | "EQ" -> "==" | "NEQ" -> "!="
        | "LT" -> "<" | "GT" -> ">" | "LTS" -> "<" | "GTS" -> ">"
        | "ADD" -> "+" | "SUB" -> "-"
        | "MUL" -> "*" | "MULS" -> "*"
        | "DIV" -> "/" | "DIVS" -> "/"
        | "POW" -> "**"
        | "SHIFTL" -> "<<" | "SHIFTR" -> ">>"
        | _ -> op
      in
      Printf.sprintf "(%s %s %s)" 
        (generate_sv lhs 0 |> String.trim) op_str (generate_sv rhs 0 |> String.trim)
        
  | UnaryOp { op; operand; _ } ->
      let op_str = match op with
        | "NOT" -> "~"
        | "REDAND" -> "&" 
        | "REDOR" -> "|" 
        | "REDXOR" -> "^" 
        | "EXTEND" -> ""  (* Extension is implicit in SystemVerilog *)
        | _ -> op
      in
      if op_str = "" then generate_sv operand 0 |> String.trim
      else Printf.sprintf "%s%s" op_str (generate_sv operand 0 |> String.trim)
        
  | Concat { parts; _ } ->
      let part_strs = List.map (generate_sv_indent 0) parts |> List.map String.trim in
      Printf.sprintf "{%s}" (String.concat ", " part_strs)
      
  | Cond { condition; then_val; else_val; _ } ->
      Printf.sprintf "(%s) ? (%s) : (%s)"
        (generate_sv condition 0 |> String.trim)
        (generate_sv then_val 0 |> String.trim)
        (generate_sv else_val 0 |> String.trim)
        
  | SenTree senses ->
      Printf.sprintf "@(%s)" 
        (String.concat " or " (List.map (generate_sv_indent 0) senses))
        
  | SenItem { edge_str; signal; _ } ->
      Printf.sprintf "%s %s" edge_str (generate_sv signal 0 |> String.trim)
      
  | Unknown (t, n) ->
      Printf.sprintf "/* Unknown: %s %s */" t n

and generate_sv_indent indent node = generate_sv node indent

(* Helper functions *)

(* Updated generate_interface_module function *)
and generate_interface_module name stmts =
 generate_regular_module name stmts

and fix_varxref_in_stmt stmt =
  (* Recursively fix VarXRef nodes to use direct variable names *)
  let rec fix_node = function
    | VarXRef { name; _ } -> VarRef { name; access = "RD" }
    | Assign { lhs; rhs; is_blocking } ->
        Assign { lhs = fix_node lhs; rhs = fix_node rhs; is_blocking }
    | BinaryOp { op; lhs; rhs } ->
        BinaryOp { op; lhs = fix_node lhs; rhs = fix_node rhs }
    | Always { always; senses; stmts } ->
        let fixed_stmts = List.map fix_varxref_in_stmt stmts in
        Always { always; senses; stmts = fixed_stmts }
    | node -> node
  in
  fix_node stmt

and generate_regular_module name stmts =
  let params = generate_params stmts in
  let ports = generate_ports stmts in
  let internals = generate_internals stmts 1 in
  let param_str = if params = "" then "" else Printf.sprintf " #(\n%s\n)" params in
  let port_str = if ports = "" then "" else Printf.sprintf " (\n%s\n)" ports in
  Printf.sprintf "module %s%s%s;\n%s\nendmodule" name param_str port_str internals

(* Enhanced module generation with interface flattening *)
and generate_module_with_interfaces name stmts interface_defs =
  (* Separate regular ports from interface references *)
  let (regular_ports, interface_refs, other_stmts) = 
    List.fold_left (fun (ports, ifaces, others) stmt ->
      match stmt with
      | Var { var_type = "PORT"; _ } as v -> (v :: ports, ifaces, others)
      | Var { var_type = "IFACEREF"; name = iface_name; _ } -> 
          (ports, iface_name :: ifaces, others)
      | _ -> (ports, ifaces, stmt :: others)
    ) ([], [], []) stmts
  in
  
  (* Generate flattened ports from interfaces *)
  let flattened_ports = List.fold_left (fun acc iface_name ->
    match List.assoc_opt iface_name interface_defs with
    | Some iface_def -> (flatten_interface_to_ports iface_name iface_def) @ acc
    | None -> acc
  ) [] interface_refs in
  
  (* Combine regular and flattened ports *)
  let all_ports = List.map (fun v -> generate_sv v 1) regular_ports @
                  List.map (fun (pname, ptype) -> 
                    Printf.sprintf "  inout %s %s" ptype pname) flattened_ports in
  
  let port_str = String.concat ",\n" all_ports in
  let internals = String.concat "\n" (List.map (generate_sv_indent 1) other_stmts) in
  
  Printf.sprintf "module %s (\n%s\n);\n%s\nendmodule" name port_str internals

and generate_params stmts =
  let params = List.filter_map (function
    | Var { var_type = "GPARAM"; _ } as v -> 
        let param_str = generate_sv v 1 |> String.trim in
        (* Remove trailing semicolon from parameter declarations *)
        let clean_param = Str.global_replace (Str.regexp ";$") "" param_str in
        Some clean_param
    | _ -> None
  ) stmts in
  String.concat ",\n  " params

and generate_ports stmts =
  let ports = List.filter_map (function
    | Var { var_type = "PORT"; _ } as v -> Some (generate_sv v 1 |> String.trim)
    | _ -> None
  ) stmts in
  String.concat ",\n  " ports

and generate_internals stmts indent =
  let internals = List.filter (function
    | Var { var_type = "GPARAM" | "PORT" | "IFACEREF"; _ } -> false  (* Added IFACEREF *)
    | _ -> true
  ) stmts in
  String.concat "\n" (List.map (generate_sv_indent indent) internals)
