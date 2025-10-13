(* sv_main_yosys.ml - Main translation with Yosys compatibility *)
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
  
  (* Open warnings file *)
  let warnings_file = rslt ^ "synthesis_warnings.txt" in
  let warnings_fd = open_out warnings_file in
  
  List.iter (fun itm ->
    Printf.fprintf stderr "Processing %s...\n" itm;
    let ast = translate_tree_to_ast (obj^itm) in
    Hashtbl.add asthash itm ast;
    
    (* Generate with warnings *)
    let (result, warnings) = Sv_gen_yosys.generate_sv_with_warnings ast 0 in
    
    (* Write output *)
    let out_file = rslt^"decompile_"^itm^".sv" in
    let fd = open_out out_file in
    output_string fd result;
    close_out fd;
    
    (* Write warnings for this file *)
    if List.length warnings > 0 then begin
      Printf.fprintf warnings_fd "\n=== %s ===\n" itm;
      List.iter (fun w -> Printf.fprintf warnings_fd "  WARNING: %s\n" w) warnings;
      Printf.fprintf stderr "  %d warnings generated\n" (List.length warnings)
    end else begin
      Printf.fprintf stderr "  No warnings\n"
    end
  ) !lst;
  
  close_out warnings_fd;
  Printf.printf "\nAll warnings written to: %s\n" warnings_file

(* Alternative: Process single file with warnings to stdout *)
let process_single_file json_file output_file =
  Printf.fprintf stderr "Processing %s...\n" json_file;
  let ast = translate_tree_to_ast json_file in
  let (result, warnings) = Sv_gen_yosys.generate_sv_with_warnings ast 0 in
  
  (* Write output *)
  let fd = open_out output_file in
  output_string fd result;
  close_out fd;
  
  (* Print warnings *)
  if List.length warnings > 0 then begin
    Printf.fprintf stderr "\n=== SYNTHESIS WARNINGS ===\n";
    List.iter (fun w -> Printf.fprintf stderr "  WARNING: %s\n" w) warnings;
    Printf.fprintf stderr "\nTotal: %d warnings\n" (List.length warnings)
  end else begin
    Printf.fprintf stderr "No warnings - code appears synthesizable\n"
  end

(* Main entry point *)
let () =
  if Array.length Sys.argv > 1 then begin
    let mode = Sys.argv.(1) in
    match mode with
    | "scan" when Array.length Sys.argv > 2 ->
        let output_dir = Sys.argv.(2) in
        scan output_dir
    | "file" when Array.length Sys.argv > 3 ->
        let json_file = Sys.argv.(2) in
        let output_file = Sys.argv.(3) in
        process_single_file json_file output_file
    | _ ->
        Printf.eprintf "Usage:\n";
        Printf.eprintf "  %s scan <output_dir>  - Process all files in obj_dir/\n" Sys.argv.(0);
        Printf.eprintf "  %s file <json> <out>  - Process single file\n" Sys.argv.(0);
        exit 1
  end else begin
    (* Default behavior - scan current directory *)
    scan "rslt/"
  end
