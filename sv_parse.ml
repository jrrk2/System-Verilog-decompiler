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
      let base = json |> member "refDTypep" |> to_string_option |> Option.value ~default:"" in
      (* FIXED: Use declRange if available, otherwise parse rangep *)
      let range = match json |> member "declRange" |> to_string_option with
        | Some decl_range when decl_range <> "" -> 
            (* Remove brackets from "[0:3]" to get "0:3" *)
            if String.length decl_range > 2 then
              String.sub decl_range 1 (String.length decl_range - 2)
            else
              ""
        | _ ->
            (* Fall back to parsing rangep array *)
            let range_json = json |> member "rangep" |> to_list in
            (match range_json with
            | r :: _ -> 
                let left = r |> member "leftp" |> to_list |> List.hd |> member "name" |> to_string_option |> Option.value ~default:"0" in
                let right = r |> member "rightp" |> to_list |> List.hd |> member "name" |> to_string_option |> Option.value ~default:"0" in
                Printf.sprintf "%s:%s" left right
            | _ -> "")
      in
      ArrayType' { base; range }

  | "PACKARRAYDTYPE" ->
      let base = json |> member "refDTypep" |> to_string_option |> Option.value ~default:"" in
      (* FIXED: Use declRange if available, otherwise parse rangep *)
      let range = match json |> member "declRange" |> to_string_option with
        | Some decl_range when decl_range <> "" -> 
            (* Remove brackets from "[0:3]" to get "0:3" *)
            if String.length decl_range > 2 then
              String.sub decl_range 1 (String.length decl_range - 2)
            else
              ""
        | _ ->
            (* Fall back to parsing rangep array *)
            let range_json = json |> member "rangep" |> to_list in
            (match range_json with
            | r :: _ -> 
                let left = r |> member "leftp" |> to_list |> List.hd |> member "name" |> to_string_option |> Option.value ~default:"0" in
                let right = r |> member "rightp" |> to_list |> List.hd |> member "name" |> to_string_option |> Option.value ~default:"0" in
                Printf.sprintf "%s:%s" left right
            | _ -> "")
      in
      PackArrayType' { base; range }

  | "IFACEREFDTYPE" ->
      let ifacename = json |> member "ifaceName" |> to_string_option |> Option.value ~default:"" in
      let modportname = json |> member "modportName" |> to_string_option |> Option.value ~default:"" in
      let ifacep = json |> member "ifacep" |> to_string_option |> Option.value ~default:"" in
      let modportp = json |> member "modportp" |> to_string_option |> Option.value ~default:"" in
      IntfRefType' { ifacename;
      modportname;
      ifacep;
      modportp }

  | "STRUCTDTYPE" ->
      let name = json |> member "name" |> to_string in
      let packed = json |> member "packed" |> to_bool in
      let members = json |> member "membersp" |> to_list |> List.map (parse_type attr) in
      StructType { name; packed; members }

  | "UNIONDTYPE" ->
      let name = json |> member "name" |> to_string in
      let packed = json |> member "packed" |> to_bool in
      let members = json |> member "membersp" |> to_list |> List.map (parse_type attr) in
      UnionType { name; packed; members }

  | "MEMBERDTYPE" ->
      let name = json |> member "name" |> to_string in
      let dtype = json |> member "dtypep" |> to_string in
      let child = json |> member "childDTypep" |> to_list |> List.map (parse_type attr) in
      let value = json |> member "valuep" |> to_list |> List.map (parse_type attr) in
      MemberType' { name; dtype; child; value }

  | "CONSTDTYPE" ->
      let name = json |> member "name" |> to_string in
      let dtype = json |> member "dtypep" |> to_string in
      let child = json |> member "childDTypep" |> to_list |> List.map (parse_type attr) in
      ConstType' { name; dtype; child }

  | oth -> UnknownType (node_type, json )

let rec extract_const_value = function
  | `Assoc fields ->
      (try
        let name = List.assoc "name" fields |> to_string in
        name
      with _ -> "/* const value */")
  | _ -> "/* const value */"
  
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
let depth = ref []

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
      Cell' { name; modp_addr; pins }
      
  | "PIN" ->
      let expr = try Some (json |> member "exprp" |> to_list |> List.hd |> parse' attr name) with _ -> None in
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
	  | v :: _ -> 
	      (* Try to extract constant value directly *)
	      (try
		let const_name = v |> member "name" |> to_string in
		Some (Const { name = const_name; dtype_ref = None })
	      with _ ->
		Some (parse' attr name v))
	  | [] -> None
	with _ -> None
      in
      let v = Var' { name; dtype_ref; var_type; direction; value; dtype_name; is_param } in
      if var_type = "IFACEREF" then List.iter (fun nam -> Hashtbl.add attr.var_table nam v) [name;dtype_ref;addr];
      v            
									    
  | "INITITEM" ->
      let value = json |> member "valuep" |> to_list |> List.map (parse' attr name) in
      InitItem { value }
									    
  | "CONST" ->
      let dtype_ref = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      Const' { name; dtype_ref }
      
  | "TYPEDEF" ->
      let dtype_ref = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      Typedef' { name; dtype_ref }
      
  | "FUNC" ->
      let dtype_ref = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let vars = json |> member "fvarp" |> to_list |> List.map (parse' attr name) in
      Func' { name; dtype_ref; stmts; vars }
      
  | "TASK" ->
      let dtype_ref = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let vars = json |> member "fvarp" |> to_list |> List.map (parse' attr name) in
      Task' { name; dtype_ref; stmts; vars }
      
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
      
  | "INSIDERANGE" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      InsideRange { lhs; rhs }
      
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
(*      *)
  | "WHILE" ->
      let condition = json |> member "condp" |> to_list |> List.hd |> parse' attr name in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let incs = json |> member "incsp" |> to_list |> List.map (parse' attr name) in
      While { condition; stmts; incs }
(* *)      
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
      
  | "TASKREF" ->
      let args = json |> member "pinsp" |> to_list |> 
        List.filter_map (fun pin -> 
          try Some (pin |> member "exprp" |> to_list |> List.hd |> parse' attr name)
          with _ -> None
        ) in
      TaskRef { name; args }

  | "RAND" ->
      let name = json |> member "name" |> to_string in
      let args = json |> member "seedp" |> to_list |> List.map (parse' attr name) in
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
  | "AND" | "OR" | "XOR"
  | "EQ" | "NEQ" | "LT" | "LTE" | "LTES" | "GT" | "GTE" | "GTES" | "LTS" | "GTS"
  | "EQWILD" | "NEQWILD" | "NEQCASE"
  | "ADD" | "SUB" | "MUL" | "MULS" | "DIV" | "DIVS" | "POW" | "POWSU" | "SHIFTL" | "SHIFTR" | "SHIFTRS" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      BinaryOp { op = node_type; lhs; rhs }
      
  (* Unary operators *)
  | "NOT" | "REDAND" | "REDOR" | "REDXOR" | "EXTEND" | "LOGNOT" | "ONEHOT" | "ONEHOT0" | "NEGATE"
  | "EXTENDS" | "ISUNKNOWN" | "CLOG2" ->
      let operand = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      UnaryOp { op = node_type; operand }
  | "EVENTCONTROL" ->
      let sense = json |> member "sensesp" |> to_list |> List.map (parse' attr name) in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      EventCtrl {sense; stmts}
  | "INITARRAY" ->
      let inits = json |> member "initsp" |> to_list |> List.map (parse' attr name) in
      InitArray {inits}

  | "INITIAL" ->
      let suspend = json |> member "isSuspendable" |> to_bool in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      Initial {suspend; stmts}

  | "INITIALSTATIC" ->
      let suspend = json |> member "isSuspendable" |> to_bool in
      let process = json |> member "needProcess" |> to_bool in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      InitialStatic {suspend; process; stmts}

  | "FINAL" ->
      let suspend = json |> member "isSuspendable" |> to_bool in
      let process = json |> member "needProcess" |> to_bool in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      Final {suspend; process; stmts}

  | "FINISH" ->
      Finish

  | "DELAY" ->
      let cycle = json |> member "isCycleDelay" |> to_bool in
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      Delay {cycle; lhs; stmts}

  | "ITORD" ->
      let dtype = json |> member "dtypep" |> to_string in
      let lhs = json |> member "lhsp" |> to_list |> List.map (parse' attr name) in
      Itord' {dtype; lhs}

  | "CVTPACKSTRING" ->
      let dtype = json |> member "dtypep" |> to_string in
      let lhs = json |> member "lhsp" |> to_list |> List.map (parse' attr name) in
      CvtPackString' {dtype; lhs}

  | "FOPEN" ->
      let dtype = json |> member "dtypep" |> to_string in
      let filename = json |> member "filenamep" |> to_list |> List.map (parse' attr name) in
      let mode = json |> member "modep" |> to_list |> List.map (parse' attr name) in
      Fopen' {dtype; filename; mode}

  | "FCLOSE" ->
      let file = json |> member "filep" |> to_list |> List.map (parse' attr name) in
      Fclose {file}

  | "DISPLAY" ->
      let fmt = json |> member "fmtp" |> to_list |> List.map (parse' attr name) in
      let file = json |> member "filep" |> to_list |> List.map (parse' attr name) in
      Display {fmt; file}

  | "SFORMAT" ->
      let fmt = json |> member "fmtp" |> to_list |> List.map (parse' attr name) in
      let lhs = json |> member "lhsp" |> to_list |> List.map (parse' attr name) in
      Sformat {fmt; lhs}

  | "SFORMATF" ->
      let expr = json |> member "exprsp" |> to_list |> List.map (parse' attr name) in
      let scope = json |> member "scopeNamep" |> to_list |> List.map (parse' attr name) in
      Sformatp {expr; scope}

  | "SCOPENAME" ->
      let dtype = json |> member "dtypep" |> to_string in
      ScopeName {dtype}

  | "TIME" ->
      let dtype = json |> member "dtypep" |> to_string in
      Time {dtype}

  | "TEXT" ->
      let text = json |> member "shortText" |> to_string in
      Text {text}

  | "SAMPLED" ->
      let dtype = json |> member "dtypep" |> to_string in
      let expr = json |> member "exprp" |> to_list |> List.map (parse' attr name) in
      Sampled {dtype; expr}

  | "CEXPR" ->
      let dtype = json |> member "dtypep" |> to_string in
      let expr = json |> member "exprsp" |> to_list |> List.map (parse' attr name) in
      Cexpr {dtype; expr}

  | "STMTEXPR" ->
      let expr = json |> member "exprp" |> to_list |> List.hd |> parse' attr name in
     StmtExpr {expr}

  | "CONSPACKUORSTRUCT" ->
      let dtype = json |> member "dtypep" |> to_string in
      let members = json |> member "membersp" |> to_list |> List.map (parse' attr name) in
     ConsPack' {dtype; members}

  | "CONSPACKMEMBER" ->
      let dtype = json |> member "dtypep" |> to_string in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
     ConsPackMember' {dtype; rhs}

  | "VALUEPLUSARGS" ->
      let dtype = json |> member "dtypep" |> to_string in
      let search = json |> member "searchp" |> to_list |> List.map (parse' attr name) in
      let out = json |> member "outp" |> to_list |> List.map (parse' attr name) in
     ValuePlusArgs' {dtype; search; out}

  | "TESTPLUSARGS" ->
      let dtype = json |> member "dtypep" |> to_string in
      let search = json |> member "searchp" |> to_list |> List.map (parse' attr name) in
     TestPlusArgs' {dtype; search}

  | "REPLICATE" ->
      let dtype = json |> member "dtypep" |> to_string in
      let src = json |> member "srcp" |> to_list |> List.hd |> parse' attr name in
      let count = json |> member "countp" |> to_list |> List.hd |> parse' attr name in
     Replicate' {dtype; src; count}

  | "JUMPBLOCK" ->
      let stmt = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
     JumpBlock { stmt }

  | "JUMPGO" ->
      let label = json |> member "labelp" |> to_string in
      JumpGo' { label }

  | "STOP" ->
      let fatal = json |> member "isFatal" |> to_bool in
      Stop { fatal }

  | "CMETHODHARD" ->
      let dtype = json |> member "dtypep" |> to_string in
      let from = json |> member "fromp" |> to_list |> List.hd |> parse' attr name in
      let pins = json |> member "pinsp" |> to_list |> List.map (parse' attr name) in
      CMethodHard' { dtype; from; pins }
 
  | oth -> Unknown (node_type, json)

  and parse' attr name json =
     depth := json :: !depth;
     let rslt = parse_json ({attr with parent=name::attr.parent}) json in
     depth := List.tl !depth;
     rslt

let othrw = ref None
let othrwtyp = ref None

let rec rw attr = function
| Netlist lst -> Netlist (List.map (rw attr) lst)
| Module {name; stmts} -> Module {name; stmts=List.map (rw attr) stmts}
| Var' {name; dtype_ref; var_type; direction; value; dtype_name; is_param} ->
  let typ = rwtyp' attr (Hashtbl.find_opt attr.type_table dtype_ref) in
  Var {name; dtype_ref=typ; var_type; direction; value; dtype_name; is_param}
| Cell' {name; modp_addr; pins } ->
  Cell {name; modp_addr=rwopt attr (Hashtbl.find_opt attr.module_table modp_addr); pins=List.map (rw attr) pins}
| Pin {name; expr} -> Pin {name; expr=rwopt attr expr}
| InsideRange {lhs; rhs} -> InsideRange {lhs=rw attr lhs; rhs=rw attr rhs}
| Assign {lhs; rhs; is_blocking} -> Assign {lhs=rw attr lhs; rhs=rw attr rhs; is_blocking}
| AssignW {lhs; rhs} -> AssignW {lhs=rw attr lhs; rhs=rw attr rhs}
| VarRef {name; access} as v -> v
| VarXRef {name; access; dotted} as v -> v
| Always {always; senses; stmts} -> Always {always; senses=List.map (rw attr) senses; stmts=List.map (rw attr) stmts};
| SenTree lst -> SenTree (List.map (rw attr) lst)
| SenItem {edge_str; signal} -> SenItem {edge_str; signal=rw attr signal}
| BinaryOp {op; lhs; rhs} -> BinaryOp {op; lhs=rw attr lhs; rhs=rw attr rhs}
| Interface {name; params; stmts} ->
  Interface {name; params=List.map (rw attr) params; stmts=List.map (rw attr) stmts}
| Modport { name; vars } -> Modport { name; vars=List.map (rw attr) vars }
| ModportVarRef { name; direction; var_ref } -> ModportVarRef { name; direction; var_ref }
| Const' { name; dtype_ref } ->
  Const { name; dtype_ref=rwtyp' attr (Hashtbl.find_opt attr.type_table dtype_ref) }
| Begin { name; stmts; is_generate } -> Begin { name; stmts=List.map (rw attr) stmts; is_generate }
| Sel { expr; lsb; width; range } -> Sel { expr=rw attr expr; lsb=rwopt attr lsb; width=rwopt attr width; range }
| Case {expr; items} -> Case {expr = rw attr expr; items = List.map (rwitm attr) items}
| EventCtrl { sense; stmts} -> EventCtrl {sense = List.map (rw attr) sense; stmts = List.map (rw attr) stmts}
| InitArray { inits} -> InitArray {inits = List.map (rw attr) inits}
| Initial { suspend; stmts} -> Initial {suspend; stmts = List.map (rw attr) stmts}
| InitialStatic { suspend; process; stmts} -> InitialStatic {suspend; process; stmts = List.map (rw attr) stmts}
| Final { suspend; process; stmts} -> Final {suspend; process; stmts = List.map (rw attr) stmts}
| Finish -> Finish
| Delay { cycle; lhs; stmts} -> Delay {cycle; lhs = rw attr lhs; stmts = List.map (rw attr) stmts}
| While { condition; stmts; incs} -> While {condition = rw attr condition;
stmts = List.map (rw attr) stmts;
incs = List.map (rw attr) incs;}
| UnaryOp {op; operand} -> UnaryOp { op; operand = rw attr operand; }
| Cond {condition; then_val; else_val } -> Cond {condition = rw attr condition;
    then_val = rw attr then_val;
    else_val = rw attr else_val} 
| If {condition; then_stmt; else_stmt } -> If {condition = rw attr condition;
    then_stmt = rw attr then_stmt;
    else_stmt = match else_stmt with Some stmt -> Some (rw attr stmt) | None -> None} 
| ArraySel { expr; index } -> ArraySel { expr = rw attr expr; index = rw attr index }
| FuncRef { name; args } -> FuncRef { name; args = List.map (rw attr) args }
| TaskRef { name; args } -> TaskRef { name; args = List.map (rw attr) args }
| Concat { parts } -> Concat { parts = List.map (rw attr) parts }
| Display { fmt; file } -> Display { fmt=List.map (rw attr) fmt; file = List.map (rw attr) file }
| Sformat { fmt; lhs } -> Sformat { fmt=List.map (rw attr) fmt; lhs = List.map (rw attr) lhs }
| Sformatp { expr; scope } -> Sformatp { expr=List.map (rw attr) expr; scope = List.map (rw attr) scope }
| ScopeName { dtype } -> ScopeName { dtype }
| Time { dtype } -> Time { dtype }
| Text { text } -> Text { text }
| Sampled { dtype; expr } -> Sampled { dtype; expr = List.map (rw attr) expr }
| Cexpr { dtype; expr } -> Cexpr { dtype; expr = List.map (rw attr) expr }
| StmtExpr { expr } -> StmtExpr { expr = rw attr expr }
| Package { name; stmts } -> Package { name; stmts = List.map (rw attr) stmts }
| Typedef' { name; dtype_ref } -> Typedef { name;
    dtype_ref = rwtyp' attr (Hashtbl.find_opt attr.type_table dtype_ref) }
| Typedef { name; dtype_ref } -> Typedef { name; dtype_ref } 
| Itord' { dtype; lhs } -> Itord {
    dtype_ref = rwtyp' attr (Hashtbl.find_opt attr.type_table dtype);
    lhs = List.map (rw attr) lhs }
| Itord { dtype_ref; lhs } -> Itord { dtype_ref; lhs } 
| CvtPackString' { dtype; lhs } -> CvtPackString {
    dtype_ref = rwtyp' attr (Hashtbl.find_opt attr.type_table dtype);
    lhs = List.map (rw attr) lhs }
| CvtPackString { dtype_ref; lhs } -> CvtPackString { dtype_ref; lhs } 
| Fopen' { dtype; filename; mode } -> Fopen {
    dtype_ref = rwtyp' attr (Hashtbl.find_opt attr.type_table dtype);
    filename = List.map (rw attr) filename;
    mode = List.map (rw attr) mode }
| Fopen { dtype_ref; filename; mode } -> Fopen { dtype_ref; filename; mode } 
| Fclose { file } -> Fclose {
    file = List.map (rw attr) file }
| ValuePlusArgs' { dtype; search; out } -> ValuePlusArgs {
    dtype_ref = rwtyp' attr (Hashtbl.find_opt attr.type_table dtype);
    search = List.map (rw attr) search;
    out = List.map (rw attr) out;
 }
| ValuePlusArgs _ as v -> v
| TestPlusArgs' { dtype; search } -> TestPlusArgs {
    dtype_ref = rwtyp' attr (Hashtbl.find_opt attr.type_table dtype);
    search = List.map (rw attr) search;
 }
| TestPlusArgs _ as v -> v
| Func' { name; dtype_ref; stmts; vars } -> Func {
      name;
      dtype_ref = rwtyp' attr (Hashtbl.find_opt attr.type_table dtype_ref);
      stmts = List.map (rw attr) stmts ;
      vars = List.map (rw attr) vars }
| Func {
      name;
      dtype_ref;
      stmts;
      vars } -> Func {
      name;
      dtype_ref = rwtyp' attr dtype_ref;
      stmts = List.map (rw attr) stmts;
      vars = List.map (rw attr) vars }
| JumpBlock { stmt } -> JumpBlock {
      stmt = List.map (rw attr) stmt }
| JumpGo' { label } -> JumpGo {
      label=rwtyp' attr (Hashtbl.find_opt attr.type_table label) }
| JumpGo { label } -> JumpGo { label }
| Replicate' { dtype; src; count } -> Replicate {
      dtype_ref = rwtyp' attr (Hashtbl.find_opt attr.type_table dtype);
      src = rw attr src ;
      count = rw attr count }
| Task' { name; dtype_ref; stmts; vars } -> Task {
      name;
      dtype_ref = rwtyp' attr (Hashtbl.find_opt attr.type_table dtype_ref);
      stmts = List.map (rw attr) stmts ;
      vars = List.map (rw attr) vars }
| Task {
      name;
      dtype_ref;
      stmts;
      vars } -> Task {
      name;
      dtype_ref = rwtyp' attr dtype_ref;
      stmts = List.map (rw attr) stmts;
      vars = List.map (rw attr) vars }
| ConsPack' { dtype; members } -> ConsPack {
      dtype_ref = rwtyp' attr (Hashtbl.find_opt attr.type_table dtype );
      members = List.map (rw attr) members }
| ConsPack { dtype_ref; members } -> ConsPack { dtype_ref; members = List.map (rw attr) members }
| ConsPackMember' { dtype; rhs } -> ConsPackMember {
      dtype_ref = rwtyp' attr (Hashtbl.find_opt attr.type_table dtype );
      rhs = rw attr rhs }
| ConsPackMember { dtype_ref; rhs } -> ConsPackMember { dtype_ref; rhs = rw attr rhs }
| CaseItem { conditions; stmts } -> CaseItem {
      conditions = List.map (rw attr) conditions;
      stmts = List.map (rw attr) stmts }
| InitItem { value } -> InitItem {
      value = List.map (rw attr) value }
| CMethodHard' { dtype; from; pins } -> CMethodHard {
      dtype_ref=rwtyp' attr (Hashtbl.find_opt attr.type_table dtype);
      from=rw attr from;
      pins = List.map (rw attr) pins }
| Const _
| Cell _
| Stop _
| Replicate _
| CMethodHard _
| Var _ as skip -> skip
| Unknown (_, _) as oth -> othrw := Some oth; failwith "othrw"

and rwitm attr = function
| { conditions; statements } -> {
    conditions= List.map (rw attr) conditions;
    statements = List.map (rw attr) statements }

and rwopt attr = function
| Some x -> Some (rw attr x)
| None -> None

and rwtyp' attr = function
| Some x -> Some (rwtyp attr x)
| None -> None

and rwtyp attr = function
| ArrayType' { base=base_ref; range } ->
  let base_type = rwtyp attr (try Hashtbl.find attr.type_table base_ref with Not_found -> UnknownType (base_ref, `Null)) in
  ArrayType { base = base_type; range }
| PackArrayType' { base=base_ref; range } ->
  let base_type = rwtyp attr (try Hashtbl.find attr.type_table base_ref with Not_found -> UnknownType (base_ref, `Null)) in
  PackArrayType { base = base_type; range }
| IntfRefType' { ifacename; modportname; ifacep; modportp } ->
  IntfRefType { ifacename;
  modportname;
  ifacep=Hashtbl.find_opt attr.interface_table ifacep;
  modportp=Hashtbl.find_opt attr.interface_table modportp }
| BasicType _ as t -> t
| RefType {name; resolved} as t -> t
| EnumType {name; items} as t -> t
| StructType { name; packed; members } -> StructType { name; packed; members = List.map (rwtyp attr) members }
| UnionType { name; packed; members } -> UnionType { name; packed; members = List.map (rwtyp attr) members }
| MemberType' { name; dtype; child; value } ->
    MemberType { name;
    dtype_ref = (try Some (rwtyp attr (Hashtbl.find attr.type_table dtype)) with Not_found -> None);
    child = List.map (rwtyp attr) child; value = List.map (rwtyp attr) value }
| MemberType { name; dtype_ref; child; value } ->
    MemberType { name; dtype_ref; child = List.map (rwtyp attr) child; value = List.map (rwtyp attr) value }
| ConstType' { name; dtype; child } ->
    ConstType { name;
    dtype_ref = None (* (try Some (rwtyp attr (Hashtbl.find attr.type_table dtype)) with Not_found -> None) *);
    child = List.map (rwtyp attr) child }
| ConstType { name; dtype_ref; child } ->
    ConstType { name; dtype_ref; child = List.map (rwtyp attr) child }
| VoidType { name; resolved } -> VoidType { name; resolved }
| oth -> othrwtyp := Some oth; failwith "othrwtyp"

let dbgpass1 = ref (Unknown ("",`Null))
let dbgpass2 = ref (Unknown ("",`Null))
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
  depth := json :: [];
  let ast' = parse_json attr json in
  dbgpass1 := ast';
  let ast = rw attr (rw attr ast') in
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