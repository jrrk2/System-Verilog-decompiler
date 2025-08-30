(* Usage example *)
open Sv_ast
open Sv_common

let jsontree = ref (`String "")
let dbgpass1 = ref (Unknown ("",""))
let dbgpass2 = ref (Unknown ("",""))

(* Main translation function *)
let translate_tree_to_ast json_file =
  Hashtbl.clear type_table;  (* Clear any previous state *)
  Hashtbl.clear interface_table;
  Hashtbl.clear module_table;
  Hashtbl.clear var_table;
  print_endline ("JSON: "^json_file);
  let json = match Yojson.Basic.from_file json_file with `Assoc lst -> `Assoc (List.rev lst) | oth -> oth in
  jsontree := json;
  dbgpass1 := Sv_parse.parse_json {parent=[]} json;
  Sv_parse.parse_json {parent=[]} json

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
      let ast = translate_tree_to_ast (obj^itm) in
      dbgpass2 := ast;
      let result = Sv_gen.generate_sv ast 0 in
      let fd = open_out (rslt^"decompile_"^itm^".sv") in
      output_string fd result;
      close_out fd) !lst;
    with
    | Sys_error msg -> Printf.eprintf "Error: %s\n" msg
    | Yojson.Json_error msg -> Printf.eprintf "JSON Error: %s\n" msg
    | e -> Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e));
    flush stderr
