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
  | StructType of { name: string; packed: bool; members: sv_type list }
  | RefType of { name: string; resolved: sv_type option }
  | VoidType of { name: string; resolved: sv_type option }
  | ArrayType of { base: sv_type; range: string }
  | IntfRefType of { ifacename: string; modportname: string; ifacep: sv_node option; modportp: sv_node option }
  | UnknownType of string

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
  | Cell of {
      name: string;
      modp_addr: sv_node option;
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
      var_ref: string list
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
  | Const of {
      name: string;
      dtype_ref: sv_type option;
    }
  | Typedef of {
      name: string;
      dtype_ref: sv_type option;
    }
  | Func of {
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

type attr = {parent: string list }
