open Sv_ast
open Yojson.Basic.Util

(* Parse type table entries *)
let rec parse_type attr json =
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
      let base_type = try Hashtbl.find attr.type_table base_ref with Not_found -> UnknownType base_ref in
      ArrayType { base = base_type; range }

  | "IFACEREFDTYPE" ->
      let ifacename = json |> member "ifaceName" |> to_string_option |> Option.value ~default:"" in
      let modportname = json |> member "modportName" |> to_string_option |> Option.value ~default:"" in
      let ifacep = json |> member "ifacep" |> to_string_option |> Option.value ~default:"" in
      let modportp = json |> member "modportp" |> to_string_option |> Option.value ~default:"" in
      IntfRefType { ifacename;
      modportname;
      ifacep=Hashtbl.find_opt attr.interface_table ifacep;
      modportp=Hashtbl.find_opt attr.interface_table modportp }

  | oth -> UnknownType node_type

(* Parse type table and populate global table *)
let rec parse_type_table attr json =
  let types_json = json |> member "typesp" |> to_list in
  List.iter (fun type_json ->
    let addr = type_json |> member "addr" |> to_string_option |> Option.value ~default:"" in
    let parsed_type = parse_type attr type_json in
    if addr <> "" then Hashtbl.add attr.type_table addr parsed_type;
    let modportp = type_json |> member "modportp" |> to_string_option |> Option.value ~default:"" in
    if modportp <> "" then Hashtbl.add attr.type_table modportp parsed_type;
    let varp = type_json |> member "varp" |> to_string_option |> Option.value ~default:"" in
    if varp <> "" then Hashtbl.add attr.type_table varp parsed_type
  ) types_json

let netlist = ref []

(* Parse JSON to AST with type table support *)
let rec parse_json attr json =
  let node_type = json |> member "type" |> to_string in
  let name = json |> member "name" |> to_string_option |> Option.value ~default:"" in
  match node_type with
  | "NETLIST" ->
      (* Parse type table first *)
      let misc_list = json |> member "miscsp" |> to_list in
      List.iter (fun misc ->
        if (misc |> member "type" |> to_string) = "TYPETABLE" then
          parse_type_table attr misc
      ) misc_list;
      
      let modules = json |> member "modulesp" |> to_list |> List.map (parse' attr name) in
      (* let type_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) attr.type_table [] in *)
      netlist := modules;
      Netlist modules
      
  | "MODULE" ->
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let addr = json |> member "addr" |> to_string_option |> Option.value ~default:"" in
      let m = Module { name; stmts } in
      if addr <> "" then Hashtbl.add attr.module_table addr m;
      m
      
  | "PACKAGE" ->
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      Package { name; stmts }

  | "IFACE" ->
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let addr = json |> member "addr" |> to_string_option |> Option.value ~default:"" in
      let interface_node = Interface { name; params = []; stmts } in
      if addr <> "" then (
			  Hashtbl.add attr.interface_table addr interface_node;
			  Hashtbl.add attr.module_table addr interface_node
			 );
      Hashtbl.add attr.interface_table name interface_node; (* Also store by name *)
      interface_node

  | "CELL" ->
      let pins = json |> member "pinsp" |> to_list |> List.map (parse' attr name) in
      let modp_addr = json |> member "modp" |> to_string_option |> Option.value ~default:"" in
      Cell { name; modp_addr=Hashtbl.find_opt attr.module_table modp_addr; pins }
      
  | "PIN" ->
      let expr = json |> member "exprp" |> to_list |> List.hd |> parse' attr name in
      Pin { name; expr }
      
  | "MODPORT" ->
      let vars = json |> member "varsp" |> to_list |> List.map (parse' attr name) in
      let m = Modport { name; vars } in
      let addr = json |> member "addr" |> to_string_option |> Option.value ~default:"" in
      if addr <> "" then Hashtbl.add attr.interface_table addr m;
      m
      
  | "MODPORTVARREF" ->
      let direction = json |> member "direction" |> to_string_option |> Option.value ~default:"" in
      let var_ref = json |> member "varp" |> to_string_option |> Option.value ~default:"" in
      let m = ModportVarRef { name; direction; var_ref=attr.parent } in
      Hashtbl.add attr.var_table var_ref m;
      m
            
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
          | v :: _ -> Some (parse' attr name v)
          | [] -> None
        with _ -> None
      in
      let typ = Hashtbl.find_opt attr.type_table dtype_ref in
      let v = Var { name; dtype_ref=typ; var_type; direction; value; dtype_name; is_param } in
      if var_type = "IFACEREF" then List.iter (fun nam -> Hashtbl.add attr.var_table nam v) [name;dtype_ref;addr] ;
      v
									    
  | "CONST" ->
      let dtype_ref = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      Const { name; dtype_ref=Hashtbl.find_opt attr.type_table dtype_ref }
      
  | "TYPEDEF" ->
      let dtype_ref = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      Typedef { name; dtype_ref = Hashtbl.find_opt attr.type_table dtype_ref }
      
  | "FUNC" ->
      let dtype_ref = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let vars = json |> member "fvarp" |> to_list |> List.map (parse' attr name) in
      Func { name; dtype_ref = Hashtbl.find_opt attr.type_table dtype_ref; stmts; vars }
      
  | "ALWAYS" ->
      let always = json |> member "keyword" |> to_string_option |> Option.value ~default:"always" in
      let senses = json |> member "sensesp" |> to_list |> List.map (parse' attr name) in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      Always { always; senses; stmts }
      
  | "BEGIN" ->
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let is_generate = json |> member "generate" |> to_bool_option |> Option.value ~default:false in
      Begin { name; stmts; is_generate }
      
  | "ASSIGN" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      Assign { lhs; rhs; is_blocking = true }
      
  | "ASSIGNDLY" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      Assign { lhs; rhs; is_blocking = false }
      
  | "ASSIGNW" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      AssignW { lhs; rhs }
      
  | "IF" ->
      let condition = json |> member "condp" |> to_list |> List.hd |> parse' attr name in
      let then_stmt = json |> member "thensp" |> to_list |> List.hd |> parse' attr name in
      let else_stmt = 
        try Some (json |> member "elsesp" |> to_list |> List.hd |> parse' attr name)
        with _ -> None
      in
      If { condition; then_stmt; else_stmt }
      
  | "CASE" ->
      let expr = json |> member "exprp" |> to_list |> List.hd |> parse' attr name in
      let items_json = json |> member "itemsp" |> to_list in
      let items = List.map (fun item ->
        let conditions = item |> member "condsp" |> to_list |> List.map (parse' attr name) in
        let statements = item |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
        { conditions; statements }
      ) items_json in
      Case { expr; items }
      
  | "CASEITEM" ->
      let conditions = json |> member "condsp" |> to_list |> List.map (parse' attr name) in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      CaseItem { conditions; stmts }
      
  | "WHILE" ->
      let condition = json |> member "condp" |> to_list |> List.hd |> parse' attr name in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let incs = json |> member "incsp" |> to_list |> List.map (parse' attr name) in
      While { condition; stmts; incs }
      
  | "VARREF" ->
      let access = json |> member "access" |> to_string_option |> Option.value ~default:"RD" in
      VarRef { name; access }
      
  | "VARXREF" ->
      let access = json |> member "access" |> to_string_option |> Option.value ~default:"RD" in
      let dotted = json |> member "dotted" |> to_string_option |> Option.value ~default:"." in
      VarXRef { name; access; dotted }
      
  | "SEL" ->
      let expr = json |> member "fromp" |> to_list |> List.hd |> parse' attr name in
      let lsb = try Some (json |> member "lsbp" |> to_list |> List.hd |> parse' attr name) with _ -> None in
      let width = try Some (json |> member "widthp" |> to_list |> List.hd |> parse' attr name) with _ -> None in
      let range = json |> member "declRange" |> to_string_option |> Option.value ~default:"" in
      Sel { expr; lsb; width; range }
      
  | "ARRAYSEL" ->
      let expr = json |> member "fromp" |> to_list |> List.hd |> parse' attr name in
      let index = json |> member "bitp" |> to_list |> List.hd |> parse' attr name in
      ArraySel { expr; index }
      
  | "FUNCREF" ->
      let args = json |> member "pinsp" |> to_list |> 
        List.filter_map (fun pin -> 
          try Some (pin |> member "exprp" |> to_list |> List.hd |> parse' attr name)
          with _ -> None
        ) in
      FuncRef { name; args }
      
  | "COND" ->
      let condition = json |> member "condp" |> to_list |> List.hd |> parse' attr name in
      let then_val = json |> member "thenp" |> to_list |> List.hd |> parse' attr name in
      let else_val = json |> member "elsep" |> to_list |> List.hd |> parse' attr name in
      Cond { condition; then_val; else_val }
      
  | "CONCAT" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      Concat { parts = [lhs; rhs] }
      
  | "SENTREE" ->
      let senses = json |> member "sensesp" |> to_list |> List.map (parse' attr name) in
      SenTree senses
      
  | "SENITEM" ->
      let edge = json |> member "edgeType" |> to_string_option |> Option.value ~default:"" in
      let signal = json |> member "sensp" |> to_list |> List.hd |> parse' attr name in
      let edge_str = String.lowercase_ascii edge ^ "edge" in
      SenItem { edge_str; signal }
      
  (* Binary operators *)
  | "AND" | "OR" | "XOR" | "EQ" | "NEQ" | "LT" | "GT" | "LTS" | "GTS" 
  | "ADD" | "SUB" | "MUL" | "MULS" | "DIV" | "DIVS" | "POW" | "SHIFTL" | "SHIFTR" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      BinaryOp { op = node_type; lhs; rhs }
      
  (* Unary operators *)
  | "NOT" | "REDAND" | "REDOR" | "REDXOR" | "EXTEND" ->
      let operand = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      UnaryOp { op = node_type; operand }
      
  | oth -> Unknown (node_type, name)

  and parse' attr name = parse_json ({attr with parent=name::attr.parent})

let dbgpass1 = ref (Unknown ("",""))
let dbgpass2 = ref (Unknown ("",""))
let mods = ref []
let intf = ref []
let typ = ref []
let vars = ref []

let parse json =
  let attr = 
  {
  parent=[];
  type_table = Hashtbl.create 100;
  interface_table = Hashtbl.create 20;
  module_table = Hashtbl.create 20;
  var_table = Hashtbl.create 20;
  } in
  dbgpass1 := parse_json attr json;
  let ast = parse_json attr json in
  dbgpass2 := ast;
  mods := [];
  Hashtbl.iter (fun k x -> mods := (k,x) :: !mods) attr.module_table;
  intf := [];
  Hashtbl.iter (fun k x -> intf := (k,x) :: !intf) attr.interface_table;
  typ := [];
  Hashtbl.iter (fun k x -> typ := (k,x) :: !typ) attr.type_table;
  vars := [];
  Hashtbl.iter (fun k x -> vars := (k,x) :: !vars) attr.var_table;
  ast
