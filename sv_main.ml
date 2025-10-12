(* Usage example *)
open Sv_ast

let jsontree = ref (`String "")

(* Main translation function *)
let translate_tree_to_ast json_file =
  print_endline ("JSON: "^json_file);
  let json = match Yojson.Basic.from_file json_file with `Assoc lst -> `Assoc (List.rev lst) | oth -> oth in
  jsontree := json;
  Sv_parse.parse json

let asthash = Hashtbl.create 255

let scan rslt =
      let obj = "obj_dir/" in
      (try Unix.mkdir rslt 0o750 with e -> Printf.eprintf "%s: %s\n" rslt (Printexc.to_string e));
      let lst = ref [] in
      let fd = Unix.opendir obj in
      (try while true do
	   let f = Unix.readdir(fd) in if f.[0]<>'.' then lst := f :: !lst;
	   done with End_of_file -> Unix.closedir fd);
      List.iter (fun itm ->
      let ast = translate_tree_to_ast (obj^itm) in
      Hashtbl.add asthash itm ast;
      let result = Sv_gen.generate_sv ast 0 in
      let fd = open_out (rslt^"decompile_"^itm^".sv") in
      output_string fd result;
      close_out fd) !lst
