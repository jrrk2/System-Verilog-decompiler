# Round-Trip Validation Suite

This suite validates your Verilog decompiler by feeding its output back through Verilator and comparing the resulting AST trees at the module/statement level.

## 🔄 How Round-Trip Testing Works

```
Original Verilog
     ↓
  Verilator (compile)
     ↓
Original JSON Tree ────────┐
     ↓                     │
Your Decompiler            │
     ↓                     │
SystemVerilog Output       │
     ↓                     │
Verilator (re-compile)     │
     ↓                     │
Round-trip JSON Tree       │
     ↓                     │
     └──→ COMPARE ←────────┘
              ↓
         Differences Report
```

## 🛠️ Tools in This Suite

### 1. **roundtrip_validator.ml** (OCaml)
Deep AST comparison engine that understands your internal AST structure.

**Features:**
- Module-level comparison
- Statement-by-statement diff
- Port and variable matching
- Expression tree comparison
- Similarity scoring

**Checks:**
- ✅ Module names match
- ✅ Statement counts match
- ✅ Statement types match
- ✅ Port declarations match
- ✅ Variable declarations match
- ✅ Expression structure match

### 2. **run_roundtrip_test.sh** (Bash)
Automated workflow orchestrator.

**What it does:**
1. Runs your decompiler
2. Feeds output back to Verilator
3. Collects round-trip JSON
4. Runs validator
5. Generates report

### 3. **ast_diff_viewer.py** (Python)
Visual comparison and analysis tool.

**Features:**
- Side-by-side tree view
- Statement type distribution
- Difference highlighting
- Batch processing
- JSON export

## 🚀 Quick Start

### Prerequisites

```bash
# Install Verilator
sudo apt-get install verilator  # Ubuntu/Debian
brew install verilator          # macOS

# Verify installation
verilator --version
```

### Step 1: Compile the Validator

```bash
# Compile the OCaml validator
ocamlfind ocamlc -package yojson -linkpkg \
    -I . sv_ast.ml sv_parse.ml sv_gen.ml sv_main.ml roundtrip_validator.ml \
    -o roundtrip_validator
```

### Step 2: Run Round-Trip Test

```bash
# Make script executable
chmod +x run_roundtrip_test.sh

# Run the full round-trip validation
./run_roundtrip_test.sh obj_dir/
```

This will:
- ✅ Decompile all JSON files
- ✅ Re-compile SystemVerilog to JSON
- ✅ Compare trees
- ✅ Generate `roundtrip_report.csv`

### Step 3: Analyze Results

```bash
# View detailed comparison for a specific file
python3 ast_diff_viewer.py \
    obj_dir/original_module.tree.json \
    roundtrip_json/original_module.tree.json \
    --tree

# Or batch analyze all files
python3 ast_diff_viewer.py \
    obj_dir/ \
    roundtrip_json/ \
    detailed_analysis.json
```

## 📊 Understanding the Output

### CSV Report Format

The `roundtrip_report.csv` contains:

| Column | Description |
|--------|-------------|
| Filename | Original JSON file name |
| Identical | true/false - perfect match |
| Similarity | 0.0-1.0 score |
| Differences | Total diff count |
| Module Diffs | Module-level mismatches |
| Statement Diffs | Statement count/type diffs |
| Port Diffs | Port declaration diffs |
| Var Diffs | Variable declaration diffs |
| Expr Diffs | Expression tree diffs |

### Console Output Example

```
========================================
Round-Trip Validation
========================================
Original:   obj_dir/ariane.tree.json
Round-trip: roundtrip_json/ariane.tree.json
Similarity: 95.3%

✗ DIFFERENCES FOUND: 12

Module/Package Mismatches:
  • ariane vs ariane_testharness

Statement Mismatches:
  • Count: 487 vs 485
  • Type: ALWAYS_COMB vs ALWAYS_FF

Port Mismatches:
  • /ariane/clk: direction INPUT vs INOUT

Variable Mismatches:
  • /ariane/state_q: type REG vs LOGIC
```

## 🔍 Interpreting Results

### Perfect Match (100% Similarity)
```
✓ PERFECT MATCH - No differences found!
```
Your decompiler is **lossless** for this module! 🎉

### High Similarity (≥95%)
```
Similarity: 97.5%
Differences: 8
```
Minor differences, likely due to:
- Type inference changes (e.g., `reg` → `logic`)
- Implicit vs explicit declarations
- Comment/whitespace

**Action:** Review differences, may be acceptable

### Medium Similarity (80-94%)
```
Similarity: 87.2%
Differences: 43
```
Noticeable structural changes:
- Missing statements
- Reordered declarations
- Control flow differences

**Action:** Investigate differences, likely bugs

### Low Similarity (<80%)
```
Similarity: 62.1%
Differences: 156
```
Significant information loss:
- Major structural changes
- Missing logic blocks
- Incorrect translations

**Action:** Critical bugs, requires fixing

## 🐛 Common Issues and Fixes

### Issue 1: "No round-trip file found"

**Cause:** Verilator failed to compile the decompiled SV

**Fix:**
```bash
# Manually test compilation
verilator --lint-only results/decompile_mymodule.sv

# Check for syntax errors
```

### Issue 2: Module name mismatches

**Cause:** Your decompiler adds/removes prefixes

**Example:**
```
Original:   module foo
Round-trip: module decompile_foo
```

**Fix:** Normalize module names in comparison or decompiler output

### Issue 3: Type differences

**Cause:** SystemVerilog type inference differs from original

**Example:**
```
Original:   reg [7:0] data;
Round-trip: logic [7:0] data;
```

**Note:** This is often acceptable in SystemVerilog

### Issue 4: Statement reordering

**Cause:** Your decompiler changes declaration order

**Impact:** May not affect functionality but fails strict comparison

**Fix:** Implement order-independent comparison (already partially handled)

## 📈 Tracking Progress

### Create a Quality Dashboard

```bash
#!/bin/bash
# track_roundtrip_quality.sh

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
HISTORY_DIR="roundtrip_history"
mkdir -p "$HISTORY_DIR"

# Run round-trip test
./run_roundtrip_test.sh obj_dir/

# Save results with timestamp
cp roundtrip_report.csv "$HISTORY_DIR/report_$TIMESTAMP.csv"

# Extract summary stats
PERFECT=$(tail -n +2 roundtrip_report.csv | grep -c "true")
TOTAL=$(tail -n +2 roundtrip_report.csv | wc -l)
AVG_SIM=$(tail -n +2 roundtrip_report.csv | awk -F',' '{sum+=$3} END {print sum/NR}')

# Append to history
echo "$TIMESTAMP,$PERFECT,$TOTAL,$AVG_SIM" >> roundtrip_quality_history.csv

echo "Quality Score: $PERFECT/$TOTAL ($(echo "scale=1; $AVG_SIM*100" | bc)%)"
```

### Visualize Trends

```python
import pandas as pd
import matplotlib.pyplot as plt

# Load history
history = pd.read_csv('roundtrip_quality_history.csv', 
                      names=['timestamp', 'perfect', 'total', 'avg_sim'])

# Plot
plt.figure(figsize=(12, 6))
plt.plot(history['timestamp'], history['avg_sim'] * 100)
plt.xlabel('Date')
plt.ylabel('Average Similarity (%)')
plt.title('Round-Trip Quality Over Time')
plt.xticks(rotation=45)
plt.tight_layout()
plt.savefig('quality_trend.png')
```

## 🎯 Best Practices

### 1. Test Incrementally

Don't wait to test all files at once:

```bash
# Test one module first
python3 ast_diff_viewer.py \
    obj_dir/simple_module.json \
    roundtrip_json/simple_module.json \
    --tree
```

### 2. Start with Simple Modules

Build confidence with basic modules before complex ones:
- Simple combinational logic
- Basic sequential logic
- Then: interfaces, packages, complex hierarchies

### 3. Fix Categories Systematically

Address issues by type:
1. Module/interface declarations
2. Port declarations
3. Variable declarations
4. Assignment statements
5. Control flow (if/case/while)
6. Expressions

### 4. Use Normalized Comparison

Some differences are semantically equivalent:
- `reg` vs `logic`
- `wire` vs `logic`
- Implicit vs explicit types

Consider implementing relaxed comparison modes.

### 5. Document Known Differences

Keep a file of acceptable differences:

```markdown
# known_differences.md

## Acceptable Type Changes
- reg → logic (SV equivalence)
- wire → logic (SV equivalence)

## Acceptable Structure Changes
- Declaration reordering (no functional impact)
- Implicit port directions made explicit
```

## 🔧 Advanced Usage

### Custom Comparison Rules

Modify `roundtrip_validator.ml` to add custom comparison logic:

```ocaml
(* Add to compare_nodes function *)
| (Var v1, Var v2) ->
    let diffs = ref [] in
    if v1.name <> v2.name then
      diffs := VarMismatch (...) :: !diffs;
    
    (* Add custom rule: treat reg/logic as equivalent *)
    let normalized_type t =
      if t = "reg" || t = "logic" then "logic" else t
    in
    
    if normalized_type v1.dtype_name <> normalized_type v2.dtype_name then
      diffs := VarMismatch (...) :: !diffs;
    
    List.rev !diffs
```

### Filtering Results

```bash
# Show only files with >10 differences
tail -n +2 roundtrip_report.csv | awk -F',' '$4 > 10 {print $1, $4}'

# Show files below 90% similarity
tail -n +2 roundtrip_report.csv | awk -F',' '$3 < 0.9 {print $1, $3*100"%"}'
```

## 📝 Integration with CI/CD

### GitHub Actions Example

```yaml
name: Round-Trip Validation

on: [push, pull_request]

jobs:
  roundtrip-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      
      - name: Install Verilator
        run: sudo apt-get install -y verilator
      
      - name: Setup OCaml
        uses: ocaml/setup-ocaml@v2
        with:
          ocaml-compiler: 4.14.x
      
      - name: Compile validator
        run: |
          opam install yojson
          make roundtrip_validator
      
      - name: Run round-trip tests
        run: ./run_roundtrip_test.sh obj_dir/
      
      - name: Check quality threshold
        run: |
          AVG=$(tail -n +2 roundtrip_report.csv | \
                awk -F',' '{sum+=$3} END {print sum/NR}')
          if (( $(echo "$AVG < 0.95" | bc -l) )); then
            echo "Quality below 95%: $AVG"
            exit 1
          fi
      
      - name: Upload results
        uses: actions/upload-artifact@v2
        with:
          name: roundtrip-results
          path: |
            roundtrip_report.csv
            roundtrip_json/
```

## 🆘 Getting Help

If you encounter issues:

1. **Check the logs**: Look in `roundtrip_test.log`
2. **Verify Verilator**: Ensure it can compile your SV files standalone
3. **Test single file**: Use ast_diff_viewer.py with --tree for visualization
4. **Check file formats**: Ensure JSON is valid

## 📚 Additional Resources

- [Verilator Manual](https://verilator.org/guide/latest/)
- [SystemVerilog LRM](https://ieeexplore.ieee.org/document/8299595)
- Your decompiler documentation

---

**Remember:** The goal is not necessarily 100% perfect matches for all files, but rather to identify and understand any differences so you can make informed decisions about their acceptability.
