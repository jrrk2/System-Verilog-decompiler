open Sv_ast

(* Enhanced SystemVerilog AST to Source Code Translator with Dynamic Interface Support *)

(* Global tables *)
let type_table : (string, sv_type) Hashtbl.t = Hashtbl.create 100
let interface_table : (string, sv_node) Hashtbl.t = Hashtbl.create 20
let module_table : (string, sv_node) Hashtbl.t = Hashtbl.create 20
let var_table : (string, sv_node) Hashtbl.t = Hashtbl.create 20
