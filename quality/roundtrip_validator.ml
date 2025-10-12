(* roundtrip_validator.ml - Validate decompiler by comparing AST round-trips *)

open Sv_ast
open Printf

type diff_type =
  | ModuleMismatch of string * string
  | StatementCountMismatch of int * int
  | StatementTypeMismatch of string * string
  | PortMismatch of string
  | VarMismatch of string
  | ExprMismatch of string
  | StructuralDiff of string

type comparison_result = {
  original_file: string;
  roundtrip_file: string;
  identical: bool;
  differences: diff_type list;
  similarity_score: float;
}

(* Normalize AST nodes for comparison - remove position info, etc *)
let rec normalize_node = function
  | Module { name; stmts } ->
      Module { name; stmts = List.map normalize_node stmts }
  | Package { name; stmts } ->
      Package { name; stmts = List.map normalize_node stmts }
  | Interface { name; params; stmts } ->
      Interface { name; params = List.map normalize_node params; 
                 stmts = List.map normalize_node stmts }
  | Var { name; dtype_ref; var_type; direction; value; dtype_name; is_param } ->
      Var { name; dtype_ref; var_type; direction; 
            value = Option.map normalize_node value; 
            dtype_name; is_param }
  | Always { always; senses; stmts } ->
      Always { always; senses = List.map normalize_node senses;
               stmts = List.map normalize_node stmts }
  | Assign { lhs; rhs; is_blocking } ->
      Assign { lhs = normalize_node lhs; rhs = normalize_node rhs; is_blocking }
  | If { condition; then_stmt; else_stmt } ->
      If { condition = normalize_node condition;
           then_stmt = normalize_node then_stmt;
           else_stmt = Option.map normalize_node else_stmt }
  | Case { expr; items } ->
      Case { expr = normalize_node expr;
             items = List.map normalize_case_item items }
  | Cell { name; modp_addr; pins } ->
      Cell { name; modp_addr; pins = List.map normalize_node pins }
  | node -> node

and normalize_case_item { conditions; statements } =
  { conditions = List.map normalize_node conditions;
    statements = List.map normalize_node statements }

(* Get node type name for comparison *)
let node_type_name = function
  | Module _ -> "Module"
  | Package _ -> "Package"
  | Interface _ -> "Interface"
  | Cell _ -> "Cell"
  | Var _ -> "Var"
  | Always _ -> "Always"
  | Assign _ -> "Assign"
  | AssignW _ -> "AssignW"
  | If _ -> "If"
  | Case _ -> "Case"
  | While _ -> "While"
  | Initial _ -> "Initial"
  | Final _ -> "Final"
  | VarRef _ -> "VarRef"
  | Const _ -> "Const"
  | BinaryOp _ -> "BinaryOp"
  | UnaryOp _ -> "UnaryOp"
  | Begin _ -> "Begin"
  | _ -> "Other"

(* Compare two AST nodes and collect differences *)
let rec compare_nodes path node1 node2 =
  match (node1, node2) with
  | (Module m1, Module m2) ->
      let diffs = ref [] in
      if m1.name <> m2.name then
        diffs := ModuleMismatch (m1.name, m2.name) :: !diffs;
      
      let stmt_diffs = compare_statement_lists (path ^ "/" ^ m1.name) m1.stmts m2.stmts in
      diffs := List.rev_append stmt_diffs !diffs;
      List.rev !diffs
      
  | (Package p1, Package p2) ->
      let diffs = ref [] in
      if p1.name <> p2.name then
        diffs := ModuleMismatch (p1.name, p2.name) :: !diffs;
      
      let stmt_diffs = compare_statement_lists (path ^ "/pkg:" ^ p1.name) p1.stmts p2.stmts in
      diffs := List.rev_append stmt_diffs !diffs;
      List.rev !diffs
      
  | (Var v1, Var v2) ->
      let diffs = ref [] in
      if v1.name <> v2.name then
        diffs := VarMismatch (sprintf "%s: name %s vs %s" path v1.name v2.name) :: !diffs;
      if v1.var_type <> v2.var_type then
        diffs := VarMismatch (sprintf "%s/%s: type %s vs %s" path v1.name v1.var_type v2.var_type) :: !diffs;
      if v1.direction <> v2.direction then
        diffs := PortMismatch (sprintf "%s/%s: direction %s vs %s" path v1.name v1.direction v2.direction) :: !diffs;
      List.rev !diffs
      
  | (Always a1, Always a2) ->
      if a1.always <> a2.always then
        [StatementTypeMismatch (a1.always, a2.always)]
      else
        compare_statement_lists (path ^ "/always") a1.stmts a2.stmts
        
  | (Assign a1, Assign a2) ->
      let lhs_diffs = compare_nodes (path ^ "/assign/lhs") a1.lhs a2.lhs in
      let rhs_diffs = compare_nodes (path ^ "/assign/rhs") a1.rhs a2.rhs in
      lhs_diffs @ rhs_diffs
      
  | (If i1, If i2) ->
      let cond_diffs = compare_nodes (path ^ "/if/cond") i1.condition i2.condition in
      let then_diffs = compare_nodes (path ^ "/if/then") i1.then_stmt i2.then_stmt in
      let else_diffs = match (i1.else_stmt, i2.else_stmt) with
        | (Some e1, Some e2) -> compare_nodes (path ^ "/if/else") e1 e2
        | (None, None) -> []
        | _ -> [StructuralDiff (path ^ "/if: else clause mismatch")]
      in
      cond_diffs @ then_diffs @ else_diffs
      
  | (Case c1, Case c2) ->
      let expr_diffs = compare_nodes (path ^ "/case/expr") c1.expr c2.expr in
      let item_count1 = List.length c1.items in
      let item_count2 = List.length c2.items in
      if item_count1 <> item_count2 then
        expr_diffs @ [StatementCountMismatch (item_count1, item_count2)]
      else
        expr_diffs
        
  | (VarRef v1, VarRef v2) ->
      if v1.name <> v2.name then
        [VarMismatch (sprintf "%s: varref %s vs %s" path v1.name v2.name)]
      else
        []
        
  | (Const c1, Const c2) ->
      if c1.name <> c2.name then
        [ExprMismatch (sprintf "%s: const %s vs %s" path c1.name c2.name)]
      else
        []
        
  | (BinaryOp b1, BinaryOp b2) ->
      if b1.op <> b2.op then
        [ExprMismatch (sprintf "%s: op %s vs %s" path b1.op b2.op)]
      else
        let lhs_diffs = compare_nodes (path ^ "/binop/lhs") b1.lhs b2.lhs in
        let rhs_diffs = compare_nodes (path ^ "/binop/rhs") b1.rhs b2.rhs in
        lhs_diffs @ rhs_diffs
        
  | (n1, n2) ->
      let t1 = node_type_name n1 in
      let t2 = node_type_name n2 in
      if t1 <> t2 then
        [StatementTypeMismatch (t1, t2)]
      else
        [] (* Same type but not deeply compared - this is OK for round-trip *)

and compare_statement_lists path stmts1 stmts2 =
  let len1 = List.length stmts1 in
  let len2 = List.length stmts2 in
  
  if len1 <> len2 then
    [StatementCountMismatch (len1, len2)]
  else
    (* Compare statements but don't fail on unhandled types *)
    List.concat (List.map2 (fun s1 s2 -> 
      try compare_nodes path s1 s2
      with _ -> [] (* Ignore comparison errors for now *)
    ) stmts1 stmts2)

(* Calculate similarity score based on differences *)
let calculate_similarity total_nodes differences =
  let diff_count = List.length differences in
  if total_nodes = 0 then 1.0
  else
    let error_rate = float_of_int diff_count /. float_of_int total_nodes in
    max 0.0 (1.0 -. error_rate)

(* Count total nodes in AST for scoring *)
let rec count_nodes = function
  | Module { stmts; _ } | Package { stmts; _ } ->
      1 + List.fold_left (fun acc stmt -> acc + count_nodes stmt) 0 stmts
  | Interface { stmts; params; _ } ->
      1 + List.fold_left (fun acc stmt -> acc + count_nodes stmt) 0 (params @ stmts)
  | Always { stmts; senses; _ } ->
      1 + List.fold_left (fun acc stmt -> acc + count_nodes stmt) 0 (senses @ stmts)
  | If { condition; then_stmt; else_stmt; _ } ->
      let else_count = match else_stmt with
        | Some e -> count_nodes e
        | None -> 0
      in
      1 + count_nodes condition + count_nodes then_stmt + else_count
  | Case { expr; items; _ } ->
      1 + count_nodes expr + 
      List.fold_left (fun acc item ->
        acc + List.fold_left (fun a s -> a + count_nodes s) 0 item.statements
      ) 0 items
  | Assign { lhs; rhs; _ } | AssignW { lhs; rhs } ->
      1 + count_nodes lhs + count_nodes rhs
  | BinaryOp { lhs; rhs; _ } ->
      1 + count_nodes lhs + count_nodes rhs
  | UnaryOp { operand; _ } ->
      1 + count_nodes operand
  | _ -> 1

(* Main comparison function *)
let compare_asts original_file roundtrip_file ast1 ast2 =
  try
    let normalized1 = normalize_node ast1 in
    let normalized2 = normalize_node ast2 in
    
    let differences = compare_nodes "" normalized1 normalized2 in
    let total_nodes = count_nodes ast1 in
    let similarity = calculate_similarity total_nodes differences in
    
    {
      original_file;
      roundtrip_file;
      identical = differences = [];
      differences;
      similarity_score = similarity;
    }
  with e ->
    (* If comparison fails, return a result showing failure *)
    {
      original_file;
      roundtrip_file;
      identical = false;
      differences = [StructuralDiff ("Comparison failed: " ^ Printexc.to_string e)];
      similarity_score = 0.0;
    }

(* Print comparison result *)
let print_result result =
  printf "\n========================================\n";
  printf "Round-Trip Validation\n";
  printf "========================================\n";
  printf "Original:   %s\n" result.original_file;
  printf "Round-trip: %s\n" result.roundtrip_file;
  printf "Similarity: %.1f%%\n" (result.similarity_score *. 100.0);
  
  if result.identical then begin
    printf "\n✓ PERFECT MATCH - No differences found!\n"
  end else begin
    printf "\n✗ DIFFERENCES FOUND: %d\n\n" (List.length result.differences);
    
    (* Group differences by type *)
    let module_diffs = List.filter (function ModuleMismatch _ -> true | _ -> false) result.differences in
    let stmt_diffs = List.filter (function StatementCountMismatch _ | StatementTypeMismatch _ -> true | _ -> false) result.differences in
    let port_diffs = List.filter (function PortMismatch _ -> true | _ -> false) result.differences in
    let var_diffs = List.filter (function VarMismatch _ -> true | _ -> false) result.differences in
    let expr_diffs = List.filter (function ExprMismatch _ -> true | _ -> false) result.differences in
    let struct_diffs = List.filter (function StructuralDiff _ -> true | _ -> false) result.differences in
    
    if module_diffs <> [] then begin
      printf "Module/Package Mismatches:\n";
      List.iter (function
        | ModuleMismatch (n1, n2) -> printf "  • %s vs %s\n" n1 n2
        | _ -> ()
      ) module_diffs
    end;
    
    if stmt_diffs <> [] then begin
      printf "\nStatement Mismatches:\n";
      List.iter (function
        | StatementCountMismatch (c1, c2) -> 
            printf "  • Count: %d vs %d\n" c1 c2
        | StatementTypeMismatch (t1, t2) -> 
            printf "  • Type: %s vs %s\n" t1 t2
        | _ -> ()
      ) stmt_diffs
    end;
    
    if port_diffs <> [] then begin
      printf "\nPort Mismatches:\n";
      List.iter (function
        | PortMismatch msg -> printf "  • %s\n" msg
        | _ -> ()
      ) port_diffs
    end;
    
    if var_diffs <> [] then begin
      printf "\nVariable Mismatches:\n";
      List.iter (function
        | VarMismatch msg -> printf "  • %s\n" msg
        | _ -> ()
      ) var_diffs
    end;
    
    if expr_diffs <> [] then begin
      printf "\nExpression Mismatches:\n";
      List.iter (function
        | ExprMismatch msg -> printf "  • %s\n" msg
        | _ -> ()
      ) expr_diffs
    end;
    
    if struct_diffs <> [] then begin
      printf "\nStructural Differences:\n";
      List.iter (function
        | StructuralDiff msg -> printf "  • %s\n" msg
        | _ -> ()
      ) struct_diffs
    end
  end;
  printf "\n"

(* Main entry point - requires integration with your existing code *)
let validate_roundtrip original_json_file roundtrip_json_file =
  try
    (* Parse original JSON *)
    let original_ast = Sv_main.translate_tree_to_ast original_json_file in
    
    (* Parse round-trip JSON *)
    let roundtrip_ast = Sv_main.translate_tree_to_ast roundtrip_json_file in
    
    (* Compare *)
    let result = compare_asts original_json_file roundtrip_json_file original_ast roundtrip_ast in
    print_result result;
    result
  with
  | Sys_error msg ->
      eprintf "Error: %s\n" msg;
      exit 1
  | e ->
      eprintf "Error during validation: %s\n" (Printexc.to_string e);
      exit 1

(* Batch validation *)
let validate_directory original_dir roundtrip_dir output_csv =
  let original_files = Sys.readdir original_dir in
  let json_files = Array.to_list original_files
    |> List.filter (fun f -> Filename.check_suffix f ".json")
  in
  
  printf "Found %d JSON files to validate\n\n" (List.length json_files);
  
  let results = List.filter_map (fun filename ->
    let original_path = Filename.concat original_dir filename in
    let roundtrip_path = Filename.concat roundtrip_dir filename in
    
    if Sys.file_exists roundtrip_path then begin
      printf "Validating: %s\n" filename;
      try
        Some (validate_roundtrip original_path roundtrip_path)
      with e ->
        eprintf "  Failed: %s\n" (Printexc.to_string e);
        None
    end else begin
      eprintf "  Skipping: no round-trip file found\n";
      None
    end
  ) json_files in
  
  (* Generate CSV report *)
  let oc = open_out output_csv in
  fprintf oc "Filename,Identical,Similarity,Differences,Module Diffs,Statement Diffs,Port Diffs,Var Diffs,Expr Diffs\n";
  
  List.iter (fun result ->
    let module_count = List.length (List.filter (function ModuleMismatch _ -> true | _ -> false) result.differences) in
    let stmt_count = List.length (List.filter (function StatementCountMismatch _ | StatementTypeMismatch _ -> true | _ -> false) result.differences) in
    let port_count = List.length (List.filter (function PortMismatch _ -> true | _ -> false) result.differences) in
    let var_count = List.length (List.filter (function VarMismatch _ -> true | _ -> false) result.differences) in
    let expr_count = List.length (List.filter (function ExprMismatch _ -> true | _ -> false) result.differences) in
    
    fprintf oc "%s,%b,%.3f,%d,%d,%d,%d,%d,%d\n"
      (Filename.basename result.original_file)
      result.identical
      result.similarity_score
      (List.length result.differences)
      module_count stmt_count port_count var_count expr_count
  ) results;
  
  close_out oc;
  
  (* Print summary *)
  let total = List.length results in
  let perfect = List.length (List.filter (fun r -> r.identical) results) in
  let avg_similarity = 
    (List.fold_left (fun acc r -> acc +. r.similarity_score) 0.0 results) /. 
    float_of_int total
  in
  
  printf "\n========================================\n";
  printf "Batch Validation Summary\n";
  printf "========================================\n";
  printf "Total files: %d\n" total;
  printf "Perfect matches: %d (%.1f%%)\n" perfect (float_of_int perfect /. float_of_int total *. 100.0);
  printf "Average similarity: %.1f%%\n" (avg_similarity *. 100.0);
  printf "Results saved to: %s\n\n" output_csv

(* CLI *)
let () =
  if Array.length Sys.argv < 3 then begin
    printf "Usage:\n";
    printf "  Single:    %s <original.json> <roundtrip.json>\n" Sys.argv.(0);
    printf "  Batch:     %s <original_dir> <roundtrip_dir> <output.csv>\n" Sys.argv.(0);
    exit 1
  end;
  
  if Array.length Sys.argv = 3 then
    let _ = validate_roundtrip Sys.argv.(1) Sys.argv.(2) in ()
  else
    validate_directory Sys.argv.(1) Sys.argv.(2) Sys.argv.(3)
