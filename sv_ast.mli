(* Store interface reference with modport info *)
type interface_ref = {
  interface_name: string;
  modport_name: string option;
  variable_name: string;
}

(* Type definitions from the TYPETABLE *)
type sv_type =
  | BasicType of { keyword: string; range: string option }
  | EnumType of { name: string; items: (string * string) list }
  | StructType of { name: string; packed: bool; members: sv_type list;  }
  | UnionType of { name: string; packed: bool; members: sv_type list;  }
  | MemberType' of { name: string; dtype: string; child: sv_type list; value: sv_type list;  }
  | MemberType of { name: string; dtype_ref: sv_type option; child: sv_type list; value: sv_type list;  }
  | RefType of { name: string; resolved: sv_type option }
  | VoidType of { name: string; resolved: sv_type option }
  | ArrayType' of { base: string; range: string }
  | ArrayType of { base: sv_type; range: string }
  | PackArrayType' of { base: string; range: string }
  | PackArrayType of { base: sv_type; range: string }
  | IntfRefType' of { ifacename: string; modportname: string; ifacep: string; modportp: string }
  | IntfRefType of { ifacename: string; modportname: string; ifacep: sv_node option; modportp: sv_node option }
  | ConstType' of {name: string; dtype: string; child: sv_type list }
  | ConstType of {name: string; dtype_ref: sv_type option; child: sv_type list }
  | UnknownType of string * Yojson.Basic.t

(* AST node types *)
and sv_node = 
  | Netlist of sv_node list
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
  | Cell' of {
      name: string;
      modp_addr: string;
      pins: sv_node list;
    }
  | Cell of {
      name: string;
      modp_addr: sv_node option;
      pins: sv_node list;
    }
  | Pin of {
      name: string;
      expr: sv_node option;
    }
  | Modport of {
      name: string;
      vars: sv_node list;
    }
  | ModportVarRef of {
      name: string;
      direction: string;
      var_ref: string list
    }
  | Var' of {
      name: string;
      dtype_ref: string;
      var_type: string;
      direction: string;
      value: sv_node option;
      dtype_name: string;
      is_param: bool;
    }
  | Var of {
      name: string;
      dtype_ref: sv_type option;
      var_type: string;
      direction: string;
      value: sv_node option;
      dtype_name: string;
      is_param: bool;
    }
  | Const' of {
      name: string;
      dtype_ref: string;
    }
  | Const of {
      name: string;
      dtype_ref: sv_type option;
    }
  | Typedef' of {
      name: string;
      dtype_ref: string;
    }
  | Typedef of {
      name: string;
      dtype_ref: sv_type option;
    }
  | Func' of {
      name: string;
      dtype_ref: string;
      stmts: sv_node list;
      vars: sv_node list;
    }
  | Func of {
      name: string;
      dtype_ref: sv_type option;
      stmts: sv_node list;
      vars: sv_node list;
    }
  | Task' of {
      name: string;
      dtype_ref: string;
      stmts: sv_node list;
      vars: sv_node list;
    }
  | Task of {
      name: string;
      dtype_ref: sv_type option;
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
  | Replicate' of {
      dtype: string;
      src: sv_node;
      count: sv_node;
    }
  | Replicate of {
      dtype_ref: sv_type option;
      src: sv_node;
      count: sv_node;
    }
  | InsideRange of {
      lhs: sv_node;
      rhs: sv_node;
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
  | TaskRef of {
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
  | Stop of {
      fatal: bool
    }
  | JumpBlock of {
      stmt: sv_node list;
    }
  | JumpGo' of {
      label: string
    }
  | JumpGo of {
      label: sv_type option;
    }
  | InitArray of {
      inits: sv_node list;
    }
  | InitItem of {
      value: sv_node list;
    }
  | Initial of {
      suspend: bool;
      stmts: sv_node list;
    }
  | InitialStatic of {
      suspend: bool;
      process: bool;
      stmts: sv_node list;
    }
  | Final of {
      suspend: bool;
      process: bool;
      stmts: sv_node list;
    }
  | Delay of {
      cycle: bool;
      lhs: sv_node;
      stmts: sv_node list;
    }
  | EventCtrl of {
      sense: sv_node list;
      stmts: sv_node list;
    }
  | Fopen' of {
      dtype: string;
      filename: sv_node list;
      mode: sv_node list;
    }  
  | Fopen of {
      dtype_ref: sv_type option;
      filename: sv_node list;
      mode: sv_node list;
    }  
  | Fclose of {
      file: sv_node list;
    }  
  | Itord' of {
      dtype: string;
      lhs: sv_node list;
    }  
  | Itord of {
      dtype_ref: sv_type option;
      lhs: sv_node list;
    }  
  | CvtPackString' of {
      dtype: string;
      lhs: sv_node list;
    }  
  | CvtPackString of {
      dtype_ref: sv_type option;
      lhs: sv_node list;
    }  
  | Display of {
      fmt: sv_node list;
      file: sv_node list;
    }  
  | Sformat of {
      fmt: sv_node list;
      lhs: sv_node list;
    }  
  | Sformatp of {
      expr: sv_node list;
      scope: sv_node list;
    }
  | ScopeName of {
      dtype: string;
      }
  | Time of {
      dtype: string;
      }
  | Text of {
      text: string;
      }
  | Sampled of {
      dtype: string;
      expr: sv_node list;
    }
  | Cexpr of {
      dtype: string;
      expr: sv_node list;
    }
  | StmtExpr of {
      expr: sv_node;
    }
  | CMethodHard' of {
      dtype: string;
      from: sv_node;
      pins: sv_node list;
    }
  | CMethodHard of {
      dtype_ref: sv_type option;
      from: sv_node;
      pins: sv_node list;
    }
  | ConsPack' of {
      dtype: string;
      members: sv_node list;
    }
  | ConsPack of {
      dtype_ref: sv_type option;
      members: sv_node list;
    }
  | ConsPackMember' of {
      dtype: string;
      rhs: sv_node;
    }
  | ConsPackMember of {
      dtype_ref: sv_type option;
      rhs: sv_node;
    }
  | ValuePlusArgs' of {
      dtype: string;
      search: sv_node list;
      out: sv_node list;
    }
  | ValuePlusArgs of {
      dtype_ref: sv_type option;
      search: sv_node list;
      out: sv_node list;
    }
  | TestPlusArgs' of {
      dtype: string;
      search: sv_node list;
    }
  | TestPlusArgs of {
      dtype_ref: sv_type option;
      search: sv_node list;
    }
  | Unknown of string * Yojson.Basic.t

and case_item = {
  conditions: sv_node list;
  statements: sv_node list;
}

type attr = {
  parent: string list;
  type_table : (string, sv_type) Hashtbl.t;
  interface_table : (string, sv_node) Hashtbl.t;
  module_table : (string, sv_node) Hashtbl.t;
  var_table : (string, sv_node) Hashtbl.t;
 }