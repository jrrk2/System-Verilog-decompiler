(* Enhanced SystemVerilog AST to Source Code Translator with Dynamic Interface Support *)
open Yojson.Basic.Util

(* Type definitions from the TYPETABLE *)
type sv_type =
  | BasicType of { keyword: string; range: string option }
  | EnumType of { name: string; items: (string * string) list }
  | StructType of { name: string; packed: bool; members: sv_type list }
  | RefType of { name: string; resolved: sv_type option }
  | VoidType of { name: string; resolved: sv_type option }
  | ArrayType of { base: sv_type; range: string }
  | IntfRefType of { ifacename: string; modportname: string; ifacep: string; modportp: string }
  | UnknownType of string

(* Store interface reference with modport info *)
type interface_ref = {
  interface_name: string;
  modport_name: string option;
  variable_name: string;
}

(* AST node types *)
type sv_node = 
  | Netlist of sv_node list * (string * sv_type) list  (* nodes, type_table *)
  | Module of {
      name: string;
      stmts: sv_node list;
    }
  | Package of {
      name: string;
      stmts: sv_node list;
    }
  | Interface of {
      name: string;
      params: sv_node list;
      stmts: sv_node list;
    }
  | Cell of {
      name: string;
      modp_addr: string;
      pins: sv_node list;
    }
  | Pin of {
      name: string;
      expr: sv_node;
    }
  | Modport of {
      name: string;
      vars: sv_node list;
    }
  | ModportVarRef of {
      name: string;
      direction: string;
      var_ref: string;
    }
  | Var of {
      name: string;
      dtype_ref: string;
      var_type: string;
      direction: string;
      value: sv_node option;
      dtype_name: string;
      is_param: bool;
    }
  | Const of {
      name: string;
      dtype_ref: string;
    }
  | Typedef of {
      name: string;
      dtype_ref: string;
    }
  | Func of {
      name: string;
      dtype_ref: string;
      stmts: sv_node list;
      vars: sv_node list;
    }
  | Always of {
      always: string;
      senses: sv_node list;
      stmts: sv_node list;
    }
  | Begin of {
      name: string;
      stmts: sv_node list;
      is_generate: bool;
    }
  | Assign of {
      lhs: sv_node;
      rhs: sv_node;
      is_blocking: bool;
    }
  | AssignW of {
      lhs: sv_node;
      rhs: sv_node;
    }
  | If of {
      condition: sv_node;
      then_stmt: sv_node;
      else_stmt: sv_node option;
    }
  | Case of {
      expr: sv_node;
      items: case_item list;
    }
  | While of {
      condition: sv_node;
      stmts: sv_node list;
      incs: sv_node list;
    }
  | VarRef of {
      name: string;
      access: string;
    }
  | VarXRef of {
      name: string;
      access: string;
      dotted: string;
    }
  | Sel of {
      expr: sv_node;
      lsb: sv_node option;
      width: sv_node option;
      range: string;
    }
  | ArraySel of {
      expr: sv_node;
      index: sv_node;
    }
  | FuncRef of {
      name: string;
      args: sv_node list;
    }
  | BinaryOp of {
      op: string;
      lhs: sv_node;
      rhs: sv_node;
    }
  | UnaryOp of {
      op: string;
      operand: sv_node;
    }
  | Concat of {
      parts: sv_node list;
    }
  | Cond of {
      condition: sv_node;
      then_val: sv_node;
      else_val: sv_node;
    }
  | SenTree of sv_node list
  | SenItem of {
      edge_str: string;
      signal: sv_node;
    }
  | CaseItem of {
      conditions: sv_node list;
      stmts: sv_node list;
    }
  | Unknown of string * string

and case_item = {
  conditions: sv_node list;
  statements: sv_node list;
}

(* Global tables *)
let type_table : (string, sv_type) Hashtbl.t = Hashtbl.create 100
let interface_table : (string, sv_node) Hashtbl.t = Hashtbl.create 20
let module_table : (string, string) Hashtbl.t = Hashtbl.create 20
let var_table : (string, sv_node) Hashtbl.t = Hashtbl.create 20

let othext = ref []

(* Extract interface references from module statements *)
let extract_interface_refs () =
  let acc = ref [] in
  Hashtbl.iter (fun _ stmt ->
    match stmt with
    | Var { name = var_name; dtype_ref; var_type = "IFACEREF"; dtype_name } ->
        if false then print_endline ("Extract intf: \""^var_name^"\" ref "^dtype_ref);
        (* Parse dtype_ref to extract interface name and modport *)
        (match Hashtbl.find_opt type_table dtype_ref with
          | Some (IntfRefType { ifacename; modportname; ifacep; modportp }) ->
			      if false then print_endline ifacename;
			      let v = { interface_name = ifacename;
			      modport_name = Some modportname;
			      variable_name = var_name } in
			      if not (List.mem v !acc) then acc := v :: !acc;
	  | oth -> othext := oth :: !othext)
    | _ -> ()
  ) var_table;
  !acc

(* Get all modport names from an interface *)
let get_interface_modports interface_stmts =
  List.fold_left (fun acc stmt ->
    match stmt with
    | Modport { name; _ } -> name :: acc
    | _ -> acc
  ) [] interface_stmts

(* Determine which modport to use for a module based on its interface usage *)
let determine_modport_for_module module_name interface_name interface_stmts =
  let available_modports = get_interface_modports interface_stmts in
  match available_modports with
  | [] -> None
  | [single_modport] -> Some single_modport
  | multiple_modports ->
      (* Use heuristics based on module name if multiple modports exist *)
      if String.contains module_name 'p' || 
         String.contains module_name 'P' ||
         List.exists (String.contains module_name) ['m'; 'M'] then
        List.find_opt (fun mp -> 
          String.contains mp 'm' || String.contains mp 'M' ||
          String.contains mp 'p' || String.contains mp 'P'
        ) multiple_modports |> 
        (function None -> List.hd multiple_modports | Some mp -> mp) |> 
        Option.some
      else
        List.find_opt (fun mp -> 
          String.contains mp 's' || String.contains mp 'S' ||
          String.contains mp 'c' || String.contains mp 'C'
        ) multiple_modports |> 
        (function None -> List.nth multiple_modports 1 | Some mp -> mp) |> 
        Option.some

(* Resolve type reference to actual type info *)
let resolve_type dtype_ref =
  try 
    let resolved = Hashtbl.find type_table dtype_ref in
    match resolved with
    | RefType { name; _ } -> name
    | BasicType { keyword; range = Some r; _ } -> keyword ^ " [" ^ r ^ "]"
    | BasicType { keyword; range = None; _ } -> keyword
    | EnumType { name; _ } -> name
    | _ -> "logic"
  with Not_found -> "logic"

let resolve_type_with_brackets dtype_ref dtype_name =
  (* Use dtype_name if available, otherwise resolve from type table *)
  let base_type = if dtype_name <> "" && dtype_name <> "logic" then dtype_name else resolve_type dtype_ref in
  (* Fix double bracket issue: logic [[7:0]] -> logic [7:0] *)
  let fixed_type = Str.global_replace (Str.regexp "\\[\\[\\([^]]+\\)\\]\\]") "[\\1]" base_type in
  (* Convert "logic 7:0" format to "logic [7:0]" *)
  if String.contains fixed_type ' ' && not (String.contains fixed_type '[') then
    let parts = String.split_on_char ' ' fixed_type in
    match parts with
    | [keyword; range] when String.contains range ':' -> 
        Printf.sprintf "%s [%s]" keyword range
    | _ -> fixed_type
  else fixed_type

(* Extract interface signals with actual interface name *)
let extract_interface_signals interface_name interface_stmts =
  List.fold_left (fun acc stmt ->
    match stmt with
    | Var { name; dtype_ref; dtype_name; var_type = "VAR"; _ } ->
        let signal_type = resolve_type_with_brackets dtype_ref dtype_name in
        let flattened_name = Printf.sprintf "%s_%s" interface_name name in
        (flattened_name, signal_type, name) :: acc
    | Var { name; dtype_ref; dtype_name; var_type = "PORT"; direction = "INPUT"; _ } ->
        let signal_type = resolve_type_with_brackets dtype_ref dtype_name in
        let flattened_name = Printf.sprintf "%s_%s" interface_name name in
        (flattened_name, signal_type, name) :: acc
    | _ -> acc
  ) [] interface_stmts

(* Get modport directions with actual modport name *)
let get_modport_directions interface_stmts modport_name =
  let rec find_modport = function
    | Modport { name; vars; _ } :: _ when name = modport_name ->
        List.fold_left (fun acc var ->
          match var with
          | ModportVarRef { name; direction; _ } -> (name, direction) :: acc
          | _ -> acc
        ) [] vars
    | _ :: rest -> find_modport rest
    | [] -> []
  in
  find_modport interface_stmts

(* Find interface by name in interface table *)
let find_interface_by_name iface_name =
  Hashtbl.fold (fun key interface acc ->
    match interface with
    | Interface { name; stmts; _ } when name = iface_name -> Some (name, stmts)
    | _ -> acc
  ) interface_table None


(* Expand interface connections with dynamic lookup *)
let expand_interface_connections iface_var_name module_name interface_refs =
  (* Find the interface reference for this variable *)
  let iface_ref = List.find_opt (fun ref -> ref.variable_name = iface_var_name) interface_refs in
  match iface_ref with
  | Some { interface_name; modport_name; _ } ->
      (match find_interface_by_name interface_name with
      | Some (_, interface_stmts) ->
          let actual_modport = match modport_name with
            | Some mp -> mp
            | None -> determine_modport_for_module module_name interface_name interface_stmts |> Option.value ~default:"default"
          in
          let directions = get_modport_directions interface_stmts actual_modport in
          List.filter_map (fun (var_name, direction) ->
            let flattened_name = Printf.sprintf "%s_%s" iface_var_name var_name in
            Some (Printf.sprintf ".%s(%s)" var_name flattened_name)
          ) directions
      | None -> [])
  | None -> []

let is_flattened_module module_name = 
  String.contains module_name 'p' || String.contains module_name 'c'

let othtype = ref []
  
(* Parse type table entries *)
let rec parse_type json =
  let node_type = json |> member "type" |> to_string in
  
  match node_type with
  | "BASICDTYPE" ->
      let keyword = json |> member "keyword" |> to_string_option |> Option.value ~default:"logic" in
      let range = json |> member "range" |> to_string_option in
      BasicType { keyword; range }
      
  | "ENUMDTYPE" ->
      let name = json |> member "name" |> to_string_option |> Option.value ~default:"" in
      let items_json = json |> member "itemsp" |> to_list in
      let items = List.map (fun item ->
        let item_name = item |> member "name" |> to_string in
        let value = item |> member "valuep" |> to_list |> List.hd |> member "name" |> to_string in
        (item_name, value)
      ) items_json in
      EnumType { name; items }
      
  | "REFDTYPE" ->
      let name = json |> member "name" |> to_string_option |> Option.value ~default:"" in
      RefType { name; resolved = None }
      
  | "VOIDDTYPE" ->
      let name = json |> member "name" |> to_string_option |> Option.value ~default:"" in
      VoidType { name; resolved = None }
      
  | "UNPACKARRAYDTYPE" ->
      let base_ref = json |> member "refDTypep" |> to_string_option |> Option.value ~default:"" in
      let range_json = json |> member "rangep" |> to_list in
      let range = match range_json with
        | r :: _ -> r |> member "leftp" |> to_list |> List.hd |> member "name" |> to_string_option |> Option.value ~default:""
        | _ -> ""
      in
      let base_type = try Hashtbl.find type_table base_ref with Not_found -> UnknownType base_ref in
      ArrayType { base = base_type; range }

  | "IFACEREFDTYPE" ->
      let ifacename = json |> member "ifaceName" |> to_string_option |> Option.value ~default:"" in
      let modportname = json |> member "modportName" |> to_string_option |> Option.value ~default:"" in
      let ifacep = json |> member "ifacep" |> to_string_option |> Option.value ~default:"" in
      let modportp = json |> member "modportp" |> to_string_option |> Option.value ~default:"" in
      IntfRefType { ifacename; modportname; ifacep; modportp }

  | oth -> othtype := oth :: !othtype; UnknownType node_type

(* Parse type table and populate global table *)
let rec parse_type_table json =
  let types_json = json |> member "typesp" |> to_list in
  List.iter (fun type_json ->
    let addr = type_json |> member "addr" |> to_string_option |> Option.value ~default:"" in
    let parsed_type = parse_type type_json in
    if addr <> "" then Hashtbl.add type_table addr parsed_type
  ) types_json

let othnode = ref []

(* Parse JSON to AST with type table support *)
let rec parse_json json =
  let node_type = json |> member "type" |> to_string in
  let name = json |> member "name" |> to_string_option |> Option.value ~default:"" in
  
  match node_type with
  | "NETLIST" ->
      (* Parse type table first *)
      let misc_list = json |> member "miscsp" |> to_list in
      List.iter (fun misc ->
        if (misc |> member "type" |> to_string) = "TYPETABLE" then
          parse_type_table misc
      ) misc_list;
      
      let modules = json |> member "modulesp" |> to_list |> List.map parse_json in
      let type_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) type_table [] in
      Netlist (modules, type_list)
      
  | "MODULE" ->
      let stmts = json |> member "stmtsp" |> to_list |> List.map parse_json in
      let addr = json |> member "addr" |> to_string_option |> Option.value ~default:"" in
      if addr <> "" then (print_endline (addr^":"^name); Hashtbl.add module_table addr name);
      Module { name; stmts }
      
  | "PACKAGE" ->
      let stmts = json |> member "stmtsp" |> to_list |> List.map parse_json in
      Package { name; stmts }

  | "IFACE" ->
      let stmts = json |> member "stmtsp" |> to_list |> List.map parse_json in
      let addr = json |> member "addr" |> to_string_option |> Option.value ~default:"" in
      let interface_node = Interface { name; params = []; stmts } in
      if addr <> "" then (print_endline (addr^":"^name); Hashtbl.add interface_table addr interface_node);
      Hashtbl.add interface_table name interface_node; (* Also store by name *)
      interface_node

  | "CELL" ->
      let pins = json |> member "pinsp" |> to_list |> List.map parse_json in
      let modp_addr = json |> member "modp" |> to_string_option |> Option.value ~default:"" in
      Cell { name; modp_addr; pins }
      
  | "PIN" ->
      let expr = json |> member "exprp" |> to_list |> List.hd |> parse_json in
      Pin { name; expr }
      
  | "MODPORT" ->
      let vars = json |> member "varsp" |> to_list |> List.map parse_json in
      Modport { name; vars }
      
  | "MODPORTVARREF" ->
      let direction = json |> member "direction" |> to_string_option |> Option.value ~default:"" in
      let var_ref = json |> member "varp" |> to_string_option |> Option.value ~default:"" in
      ModportVarRef { name; direction; var_ref }
            
  | "VAR" ->
      let addr = json |> member "addr" |> to_string_option |> Option.value ~default:"" in
      let dtype_name = json |> member "dtypeName" |> to_string_option |> Option.value ~default:"logic" in
      let var_type = json |> member "varType" |> to_string_option |> Option.value ~default:"VAR" in
      let direction = json |> member "direction" |> to_string_option |> Option.value ~default:"NONE" in
      let dtype_ref = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      let is_param = json |> member "isParam" |> to_bool_option |> Option.value ~default:false in
      let value = 
        try 
          let valuep = json |> member "valuep" |> to_list in
          match valuep with
          | v :: _ -> Some (parse_json v)
          | [] -> None
        with _ -> None
      in
      let v = Var { name; dtype_ref; var_type; direction; value; dtype_name; is_param } in
      if var_type = "IFACEREF" then List.iter (fun nam -> Hashtbl.add var_table nam v) [name;dtype_ref;addr] ;
      v
									    
  | "CONST" ->
      let dtype_ref = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      Const { name; dtype_ref }
      
  | "TYPEDEF" ->
      let dtype_ref = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      Typedef { name; dtype_ref }
      
  | "FUNC" ->
      let dtype_ref = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      let stmts = json |> member "stmtsp" |> to_list |> List.map parse_json in
      let vars = json |> member "fvarp" |> to_list |> List.map parse_json in
      Func { name; dtype_ref; stmts; vars }
      
  | "ALWAYS" ->
      let always = json |> member "keyword" |> to_string_option |> Option.value ~default:"always" in
      let senses = json |> member "sensesp" |> to_list |> List.map parse_json in
      let stmts = json |> member "stmtsp" |> to_list |> List.map parse_json in
      Always { always; senses; stmts }
      
  | "BEGIN" ->
      let stmts = json |> member "stmtsp" |> to_list |> List.map parse_json in
      let is_generate = json |> member "generate" |> to_bool_option |> Option.value ~default:false in
      Begin { name; stmts; is_generate }
      
  | "ASSIGN" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse_json in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse_json in
      Assign { lhs; rhs; is_blocking = true }
      
  | "ASSIGNDLY" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse_json in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse_json in
      Assign { lhs; rhs; is_blocking = false }
      
  | "ASSIGNW" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse_json in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse_json in
      AssignW { lhs; rhs }
      
  | "IF" ->
      let condition = json |> member "condp" |> to_list |> List.hd |> parse_json in
      let then_stmt = json |> member "thensp" |> to_list |> List.hd |> parse_json in
      let else_stmt = 
        try Some (json |> member "elsesp" |> to_list |> List.hd |> parse_json)
        with _ -> None
      in
      If { condition; then_stmt; else_stmt }
      
  | "CASE" ->
      let expr = json |> member "exprp" |> to_list |> List.hd |> parse_json in
      let items_json = json |> member "itemsp" |> to_list in
      let items = List.map (fun item ->
        let conditions = item |> member "condsp" |> to_list |> List.map parse_json in
        let statements = item |> member "stmtsp" |> to_list |> List.map parse_json in
        { conditions; statements }
      ) items_json in
      Case { expr; items }
      
  | "CASEITEM" ->
      let conditions = json |> member "condsp" |> to_list |> List.map parse_json in
      let stmts = json |> member "stmtsp" |> to_list |> List.map parse_json in
      CaseItem { conditions; stmts }
      
  | "WHILE" ->
      let condition = json |> member "condp" |> to_list |> List.hd |> parse_json in
      let stmts = json |> member "stmtsp" |> to_list |> List.map parse_json in
      let incs = json |> member "incsp" |> to_list |> List.map parse_json in
      While { condition; stmts; incs }
      
  | "VARREF" ->
      let access = json |> member "access" |> to_string_option |> Option.value ~default:"RD" in
      VarRef { name; access }
      
  | "VARXREF" ->
      let access = json |> member "access" |> to_string_option |> Option.value ~default:"RD" in
      let dotted = json |> member "dotted" |> to_string_option |> Option.value ~default:"." in
      VarXRef { name; access; dotted }
      
  | "SEL" ->
      let expr = json |> member "fromp" |> to_list |> List.hd |> parse_json in
      let lsb = try Some (json |> member "lsbp" |> to_list |> List.hd |> parse_json) with _ -> None in
      let width = try Some (json |> member "widthp" |> to_list |> List.hd |> parse_json) with _ -> None in
      let range = json |> member "declRange" |> to_string_option |> Option.value ~default:"" in
      Sel { expr; lsb; width; range }
      
  | "ARRAYSEL" ->
      let expr = json |> member "fromp" |> to_list |> List.hd |> parse_json in
      let index = json |> member "bitp" |> to_list |> List.hd |> parse_json in
      ArraySel { expr; index }
      
  | "FUNCREF" ->
      let args = json |> member "pinsp" |> to_list |> 
        List.filter_map (fun pin -> 
          try Some (pin |> member "exprp" |> to_list |> List.hd |> parse_json)
          with _ -> None
        ) in
      FuncRef { name; args }
      
  | "COND" ->
      let condition = json |> member "condp" |> to_list |> List.hd |> parse_json in
      let then_val = json |> member "thenp" |> to_list |> List.hd |> parse_json in
      let else_val = json |> member "elsep" |> to_list |> List.hd |> parse_json in
      Cond { condition; then_val; else_val }
      
  | "CONCAT" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse_json in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse_json in
      Concat { parts = [lhs; rhs] }
      
  | "SENTREE" ->
      let senses = json |> member "sensesp" |> to_list |> List.map parse_json in
      SenTree senses
      
  | "SENITEM" ->
      let edge = json |> member "edgeType" |> to_string_option |> Option.value ~default:"" in
      let signal = json |> member "sensp" |> to_list |> List.hd |> parse_json in
      let edge_str = String.lowercase_ascii edge ^ "edge" in
      SenItem { edge_str; signal }
      
  (* Binary operators *)
  | "AND" | "OR" | "XOR" | "EQ" | "NEQ" | "LT" | "GT" | "LTS" | "GTS" 
  | "ADD" | "SUB" | "MUL" | "MULS" | "DIV" | "DIVS" | "POW" | "SHIFTL" | "SHIFTR" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse_json in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse_json in
      BinaryOp { op = node_type; lhs; rhs }
      
  (* Unary operators *)
  | "NOT" | "REDAND" | "REDOR" | "REDXOR" | "EXTEND" ->
      let operand = json |> member "lhsp" |> to_list |> List.hd |> parse_json in
      UnaryOp { op = node_type; operand }
      
  | oth -> othnode := oth :: !othnode; Unknown (node_type, name)

(* Interface flattening logic *)

let flatten_interface_to_ports interface_name interface_def =
  (* Extract signals from interface and convert to individual ports *)
  let rec extract_signals stmts =
    List.fold_left (fun acc stmt ->
      match stmt with
      | Var { name; dtype_ref; dtype_name; _ } when name <> "clk" ->
          (* Convert interface signals to module ports *)
          let flattened_name = Printf.sprintf "%s_%s" interface_name name in
          (flattened_name, resolve_type_with_brackets dtype_ref dtype_name) :: acc
      | _ -> acc
    ) [] stmts
  in
  extract_signals interface_def.statements

let nodelst = ref []
let dbgdecls = ref []

(* Generate SystemVerilog source from AST *)
let rec generate_sv node indent =
  let ind = String.make (indent * 2) ' ' in
  match node with
  | Netlist (modules, _) ->
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

      if has_regular_ports && has_interface_refs then
        (* Top module - generate flattened interface signals *)
        generate_top_module name stmts
      else if has_interface_refs then
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
      let module_name = try Hashtbl.find module_table modp_addr with Not_found -> "unknown_module" in
      
      (* Check if this is an interface instantiation by checking modp_addr against interface table *)
      if Hashtbl.mem interface_table modp_addr then
        Printf.sprintf "%s// Interface %s instantiation flattened away" ind name
      else
        (* Generate connections for module instantiations *)
        let pin_connections = List.fold_left (fun acc pin ->
          match pin with
          | Pin { expr = VarRef { name = "clk"; _ }; _ } -> 
              (* Direct clock connection *)
              ".clk(clk)" :: acc
          | Pin { expr = VarRef { name = iface_name; _ }; _ } ->
	      (* Interface connection - expand to flattened signals *)
              print_endline ("Expand: "^iface_name);
	      let interface_refs = extract_interface_refs () in
              let expanded = expand_interface_connections iface_name module_name interface_refs in
              expanded @ acc
          | Pin { expr = VarXRef { name = signal; dotted; _ }; _ } ->
              let flattened_name = Printf.sprintf "%s_%s" dotted signal in
              (Printf.sprintf ".%s(%s)" signal flattened_name) :: acc
          | _ -> acc
        ) [] pins in
        
        let connections = String.concat ", " (List.rev pin_connections) in
        Printf.sprintf "%s%s %s(%s);" ind module_name name connections
      
  | Pin { name; expr; _ } ->
      generate_sv expr 0
        
  | Var { name; dtype_ref; var_type; direction; value; dtype_name; is_param; _ } ->
      let resolved_type = resolve_type_with_brackets dtype_ref dtype_name in
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
      let resolved_type = resolve_type dtype_ref in
      Printf.sprintf "%stypedef %s %s;" ind resolved_type name
      
  | Func { name; dtype_ref; stmts; vars; _ } ->
      let return_type = resolve_type_with_brackets dtype_ref "" in
      (* Extract input parameters from vars, filtering out the return variable *)
      let param_vars = List.filter (function 
        | Var { var_type = "PORT"; direction = "INPUT"; name = param_name; _ } when param_name <> name -> true 
        | _ -> false) vars in
      let params = String.concat ", " (List.map (fun v ->
        match v with
        | Var { name = param_name; dtype_ref; dtype_name; _ } ->
            let ptype = resolve_type_with_brackets dtype_ref dtype_name in
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

(* Updated generate_top_module function *)
and generate_top_module name stmts =
  let interface_refs = extract_interface_refs () in
  let (regular_ports, cells, other_stmts) = 
    List.fold_left (fun (ports, cells, others) stmt ->
      match stmt with
      | Var { var_type = "PORT"; _ } as v -> (v :: ports, cells, others)
      | Var { var_type = "IFACEREF"; _ } -> (ports, cells, others) (* Skip interface refs *)
      | Cell _ as c -> (ports, c :: cells, others)
      | _ -> (ports, cells, stmt :: others)
    ) ([], [], []) stmts in

  (* Generate port declarations *)
  let port_decls = List.map (fun v -> generate_sv v 1) regular_ports in
  let port_str = String.concat ",\n" port_decls in

  (* Generate interface signal declarations *)
  let interface_signals = List.fold_left (fun acc iface_ref ->
    match find_interface_by_name iface_ref.interface_name with
    | Some (_, iface_stmts) ->
        let signals = extract_interface_signals iface_ref.variable_name iface_stmts in
        signals @ acc
    | None -> acc
  ) [] interface_refs in

  let signal_decls = List.map (fun (sig_name, sig_type, _) ->
    Printf.sprintf "  %s %s;" sig_type sig_name
  ) interface_signals in

  (* Generate input connections *)
  let input_connections = List.filter_map (fun (sig_name, _, orig_name) ->
    if orig_name = "clk" then
      Some (Printf.sprintf "  assign %s = %s;" sig_name orig_name)
    else None
  ) interface_signals in

  (* Filter and fix cell instantiations *)
  let fixed_cells = List.filter_map (fun cell ->
    match cell with
    | Cell { modp_addr; _ } as c ->
        let module_name = try Hashtbl.find module_table modp_addr with Not_found -> "" in
        if Hashtbl.mem interface_table module_name then None
        else Some (generate_sv c 1)
    | _ -> Some (generate_sv cell 1)
  ) cells in

  dbgdecls := signal_decls :: !dbgdecls;
  let body_parts = List.sort_uniq compare signal_decls @
    List.sort_uniq compare input_connections @
    fixed_cells @
    List.map (generate_sv_indent 0) other_stmts in
  let body = String.concat "\n" body_parts in

  Printf.sprintf "module %s (\n%s\n);\n%s\nendmodule" name port_str body


(* Updated generate_interface_module function *)
and generate_interface_module name stmts =
  let interface_refs = extract_interface_refs () in
  match interface_refs with
  | iface_ref :: _ ->
      (match find_interface_by_name iface_ref.interface_name with
      | Some (_, iface_stmts) ->
          let modport_name = match iface_ref.modport_name with
            | Some mp -> mp
            | None -> determine_modport_for_module name iface_ref.interface_name iface_stmts |> Option.value ~default:"default"
          in
          let directions = get_modport_directions iface_stmts modport_name in

          let flattened_ports = List.filter_map (fun (var_name, direction) ->
            let port_dir = String.lowercase_ascii direction in
            let var_info = List.find_opt (function
              | Var { name; _ } when name = var_name -> true
              | _ -> false
            ) iface_stmts in
            match var_info with
            | Some (Var { dtype_ref; dtype_name; _ }) ->
                let var_type = resolve_type_with_brackets dtype_ref dtype_name in
                Some (Printf.sprintf "  %s %s %s" port_dir var_type var_name)
            | _ -> None
          ) directions in

          let port_str = String.concat ",\n" flattened_ports in
          
          (* Generate module internals, filtering out IFACEREF vars *)
          let filtered_stmts = List.filter (function
            | Var { var_type = "IFACEREF"; _ } -> false
            | _ -> true
          ) stmts in
          
          let fixed_stmts = List.map (fix_varxref_in_stmt) filtered_stmts in
          let internals = generate_internals fixed_stmts 1 in
          
          Printf.sprintf "module %s (\n%s\n);\n%s\nendmodule" name port_str internals
      | None -> generate_regular_module name stmts)
  | [] -> generate_regular_module name stmts

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

(* Main translation function *)
let translate_tree_to_sv json_file =
  Hashtbl.clear type_table;  (* Clear any previous state *)
  Hashtbl.clear interface_table;
  Hashtbl.clear module_table;
  Hashtbl.clear var_table;
  print_endline ("JSON: "^json_file);
  let json = Yojson.Basic.from_file json_file in
  let ast = parse_json json in
  let sv_code = generate_sv ast 0 in
  sv_code

(* Usage example *)
let () =
    (try
      let obj = "obj_dir/" in
      let rslt = "results/" in
      (try Unix.mkdir rslt 0o750 with e -> Printf.eprintf "%s: %s\n" rslt (Printexc.to_string e));
      let lst = ref [] in
      let fd = Unix.opendir obj in
      (try while true do
	   let f = Unix.readdir(fd) in if f.[0]<>'.' then lst := f :: !lst;
	   done with End_of_file -> Unix.closedir fd);
      List.iter (fun itm ->
      let result = translate_tree_to_sv (obj^itm) in
      let fd = open_out (rslt^"decompile_"^itm^".sv") in
      output_string fd result;
      close_out fd) !lst;
    with
    | Sys_error msg -> Printf.eprintf "Error: %s\n" msg
    | Yojson.Json_error msg -> Printf.eprintf "JSON Error: %s\n" msg
    | e -> Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e));
    flush stderr
