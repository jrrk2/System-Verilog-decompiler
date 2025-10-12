(* quality_checker.ml - Quality assessment tool for Verilog decompiler *)

open Printf

type check_result = {
  check_name: string;
  passed: bool;
  details: string;
  score: float;
}

type quality_report = {
  file_name: string;
  total_score: float;
  checks: check_result list;
  errors: string list;
  warnings: string list;
}

(* Helper for substring search using Str library *)
let contains_substring str sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) str 0 in
    true
  with Not_found -> false

(* Syntax validation checks *)
let check_balanced_delimiters content =
  let count_char c str =
    String.fold_left (fun acc ch -> if ch = c then acc + 1 else acc) 0 str
  in
  let opens = count_char '(' content in
  let closes = count_char ')' content in
  let braces_open = count_char '{' content in
  let braces_close = count_char '}' content in
  let brackets_open = count_char '[' content in
  let brackets_close = count_char ']' content in
  
  let balanced = 
    opens = closes && 
    braces_open = braces_close && 
    brackets_open = brackets_close
  in
  
  let details = sprintf "Parens: %d/%d, Braces: %d/%d, Brackets: %d/%d"
    opens closes braces_open braces_close brackets_open brackets_close
  in
  
  {
    check_name = "Balanced Delimiters";
    passed = balanced;
    details;
    score = if balanced then 1.0 else 0.0;
  }

(* Check for basic module structure *)
let check_module_structure content =
  let has_module = 
    try
      let _ = Str.search_forward (Str.regexp "module[ \t]+[a-zA-Z_][a-zA-Z0-9_]*") content 0 in
      true
    with Not_found -> false
  in
  let has_endmodule = contains_substring content "endmodule" in
  
  let passed = has_module && has_endmodule in
  let details = sprintf "Has module: %b, Has endmodule: %b" has_module has_endmodule in
  
  {
    check_name = "Module Structure";
    passed;
    details;
    score = if passed then 1.0 else 0.0;
  }

(* Check for untranslated artifacts *)
let check_untranslated_artifacts content =
  let patterns = [
    ("JSON fragments", "\"type\":");
    ("Raw addresses", "0x");
    ("Unresolved refs", "UNKNOWN");
    ("Debug markers", "__Vdfg");
  ] in
  
  let found_issues = List.filter_map (fun (name, pattern) ->
    if contains_substring content pattern then
      Some (name, pattern)
    else
      None
  ) patterns in
  
  let passed = found_issues = [] in
  let details = if passed then "No untranslated artifacts"
    else String.concat ", " (List.map fst found_issues)
  in
  
  {
    check_name = "Untranslated Artifacts";
    passed;
    details;
    score = float_of_int (List.length patterns - List.length found_issues) /. float_of_int (List.length patterns);
  }

(* Check port declaration consistency *)
let check_port_declarations content =
  let input_count = 
    let re = Str.regexp "input[ \t]" in
    let rec count pos acc =
      try
        let _ = Str.search_forward re content pos in
        count (Str.match_end ()) (acc + 1)
      with Not_found -> acc
    in count 0 0
  in
  
  let output_count = 
    let re = Str.regexp "output[ \t]" in
    let rec count pos acc =
      try
        let _ = Str.search_forward re content pos in
        count (Str.match_end ()) (acc + 1)
      with Not_found -> acc
    in count 0 0
  in
  
  let total_ports = input_count + output_count in
  let has_reasonable_ports = total_ports > 0 && total_ports < 10000 in
  
  let details = sprintf "Inputs: %d, Outputs: %d, Total: %d" 
    input_count output_count total_ports in
  
  {
    check_name = "Port Declarations";
    passed = has_reasonable_ports;
    details;
    score = if has_reasonable_ports then 1.0 else 0.5;
  }

(* Check for SystemVerilog syntax validity *)
let check_sv_keywords content =
  let keywords = [
    "logic"; "always_comb"; "always_ff"; "assign";
    "module"; "endmodule"; "if"; "else"; "case"
  ] in
  
  let found_keywords = List.filter (fun kw -> contains_substring content kw) keywords in
  let ratio = float_of_int (List.length found_keywords) /. float_of_int (List.length keywords) in
  
  {
    check_name = "SystemVerilog Keywords Present";
    passed = ratio > 0.3;
    details = sprintf "Found %d/%d common keywords" 
      (List.length found_keywords) (List.length keywords);
    score = ratio;
  }

(* Check line length reasonableness *)
let check_line_lengths content =
  let lines = String.split_on_char '\n' content in
  let long_lines = List.filter (fun line -> String.length line > 500) lines in
  let very_long_lines = List.filter (fun line -> String.length line > 1000) lines in
  
  let passed = List.length very_long_lines = 0 in
  let details = sprintf "Total lines: %d, >500 chars: %d, >1000 chars: %d"
    (List.length lines) (List.length long_lines) (List.length very_long_lines) in
  
  let score = 
    if List.length very_long_lines > 0 then 0.0
    else if List.length long_lines > (List.length lines / 10) then 0.5
    else 1.0
  in
  
  {
    check_name = "Line Length Reasonableness";
    passed;
    details;
    score;
  }

(* Check for common errors *)
let check_common_errors content =
  let error_patterns = [
    ("Empty blocks", "begin\n[ \t]*end");
    ("Double semicolons", ";;");
    ("Mismatched assign", "assign.*assign");
  ] in
  
  let found_errors = List.filter_map (fun (name, pattern) ->
    try
      let _ = Str.search_forward (Str.regexp pattern) content 0 in
      Some name
    with Not_found -> None
  ) error_patterns in
  
  let passed = found_errors = [] in
  let details = if passed then "No common errors detected"
    else "Found: " ^ String.concat ", " found_errors
  in
  
  {
    check_name = "Common Error Patterns";
    passed;
    details;
    score = float_of_int (List.length error_patterns - List.length found_errors) /. 
            float_of_int (List.length error_patterns);
  }

(* Main quality check function *)
let assess_quality filename =
  try
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    
    let checks = [
      check_balanced_delimiters content;
      check_module_structure content;
      check_untranslated_artifacts content;
      check_port_declarations content;
      check_sv_keywords content;
      check_line_lengths content;
      check_common_errors content;
    ] in
    
    let total_score = 
      List.fold_left (fun acc check -> acc +. check.score) 0.0 checks 
      /. float_of_int (List.length checks)
    in
    
    let errors = List.filter_map (fun check ->
      if not check.passed && check.score < 0.5 then
        Some (check.check_name ^ ": " ^ check.details)
      else None
    ) checks in
    
    let warnings = List.filter_map (fun check ->
      if not check.passed && check.score >= 0.5 then
        Some (check.check_name ^ ": " ^ check.details)
      else None
    ) checks in
    
    {
      file_name = filename;
      total_score;
      checks;
      errors;
      warnings;
    }
  with
  | Sys_error msg -> 
      {
        file_name = filename;
        total_score = 0.0;
        checks = [];
        errors = [msg];
        warnings = [];
      }

(* Print quality report *)
let print_report report =
  printf "\n========================================\n";
  printf "Quality Report: %s\n" report.file_name;
  printf "========================================\n";
  printf "Overall Score: %.1f%%\n\n" (report.total_score *. 100.0);
  
  printf "Detailed Checks:\n";
  List.iter (fun check ->
    let status = if check.passed then "✓ PASS" else "✗ FAIL" in
    printf "  [%s] %s (%.0f%%)\n" status check.check_name (check.score *. 100.0);
    printf "      %s\n" check.details;
  ) report.checks;
  
  if report.errors <> [] then begin
    printf "\nErrors:\n";
    List.iter (printf "  ✗ %s\n") report.errors
  end;
  
  if report.warnings <> [] then begin
    printf "\nWarnings:\n";
    List.iter (printf "  ⚠ %s\n") report.warnings
  end;
  
  printf "\n"

(* Batch processing *)
let assess_directory dir_path output_csv =
  let files = Sys.readdir dir_path in
  let sv_files = Array.to_list files 
    |> List.filter (fun f -> Filename.check_suffix f ".sv")
    |> List.map (fun f -> Filename.concat dir_path f)
  in
  
  let reports = List.map assess_quality sv_files in
  
  (* Print individual reports *)
  List.iter print_report reports;
  
  (* Generate CSV summary *)
  let oc = open_out output_csv in
  fprintf oc "Filename,Overall Score,Balanced Delims,Module Structure,No Artifacts,Port Decls,SV Keywords,Line Lengths,No Errors\n";
  
  List.iter (fun report ->
    fprintf oc "%s,%.2f" (Filename.basename report.file_name) report.total_score;
    List.iter (fun check ->
      fprintf oc ",%.2f" check.score
    ) report.checks;
    fprintf oc "\n"
  ) reports;
  
  close_out oc;
  
  (* Print summary statistics *)
  let avg_score = 
    List.fold_left (fun acc r -> acc +. r.total_score) 0.0 reports
    /. float_of_int (List.length reports)
  in
  
  printf "\n========================================\n";
  printf "Summary Statistics\n";
  printf "========================================\n";
  printf "Files processed: %d\n" (List.length reports);
  printf "Average quality score: %.1f%%\n" (avg_score *. 100.0);
  printf "Results saved to: %s\n" output_csv;
  printf "\n"

(* Main entry point *)
let () =
  if Array.length Sys.argv < 2 then begin
    printf "Usage:\n";
    printf "  Single file: %s <file.sv>\n" Sys.argv.(0);
    printf "  Directory:   %s <dir> <output.csv>\n" Sys.argv.(0);
    exit 1
  end;
  
  if Array.length Sys.argv = 2 then
    (* Single file mode *)
    let report = assess_quality Sys.argv.(1) in
    print_report report
  else
    (* Directory batch mode *)
    assess_directory Sys.argv.(1) Sys.argv.(2)
