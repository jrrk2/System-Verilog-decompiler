# Verilog Decompiler Quality Assessment Tools

This suite of tools helps assess the quality of your Verilog-to-SystemVerilog conversion output **without loading large files entirely into memory**.

## 🛠️ Tools Overview

### 1. **quality_checker.ml** (OCaml)
Complete quality assessment with detailed scoring.

**Features:**
- ✅ Balanced delimiter checking (parentheses, braces, brackets)
- ✅ Module structure validation
- ✅ Detection of untranslated artifacts
- ✅ Port declaration analysis
- ✅ SystemVerilog keyword presence
- ✅ Line length validation
- ✅ Common error pattern detection

**Usage:**
```bash
# Compile
ocamlfind ocamlc -package yojson -linkpkg -o quality_checker quality_checker.ml

# Single file
./quality_checker results/decompile_module.sv

# Batch process directory
./quality_checker results/ quality_report.csv
```

### 2. **compare_conversions.sh** (Bash)
Fast shell-based checker with optional Verilator integration.

**Features:**
- 🚀 Fast batch processing
- 🔍 Verilator syntax validation (if installed)
- 📊 Real-time progress with color output
- 📝 Metrics file generation

**Usage:**
```bash
# Make executable
chmod +x compare_conversions.sh

# Run on results directory
./compare_conversions.sh results/

# Output saved to conversion_metrics.txt
```

### 3. **stream_quality_analyzer.py** (Python)
Memory-efficient line-by-line analyzer for large files.

**Features:**
- 💾 **Memory efficient** - streams files line-by-line
- 📈 Detailed statistical analysis
- 📊 JSON and CSV export
- 🎯 Comprehensive scoring system

**Usage:**
```bash
# Make executable
chmod +x stream_quality_analyzer.py

# Single file
python3 stream_quality_analyzer.py results/decompile_large_module.sv

# Directory with exports
python3 stream_quality_analyzer.py results/ quality_report.json quality_summary.csv
```

## 📊 Quality Metrics Explained

### Scoring System (0.0 - 1.0)

1. **Delimiter Balance (0-1)**
   - 1.0 = All delimiters balanced
   - 0.0 = Unbalanced delimiters

2. **Module Structure (0-1)**
   - 1.0 = Valid module...endmodule pairs
   - 0.0 = Missing or mismatched

3. **No Artifacts (0-1)**
   - 1.0 = No debug vars, unknown refs, or JSON fragments
   - 0.5 = Few artifacts (<10)
   - 0.0 = Many artifacts

4. **Port Declarations (0-1)**
   - 1.0 = Has valid port declarations
   - 0.5 = Questionable port count
   - 0.0 = No ports

5. **SystemVerilog Features (0-1)**
   - Based on presence of SV-specific keywords

6. **No Common Errors (0-1)**
   - Checks for empty blocks, double semicolons, etc.

### Overall Score
Average of all individual scores × 100 = percentage

**Interpretation:**
- ✓ **≥80%** = High quality, production-ready
- ⚠ **50-79%** = Acceptable, needs review
- ✗ **<50%** = Poor quality, requires fixes

## 🚀 Recommended Workflow

### Step 1: Quick Check (Bash)
```bash
./compare_conversions.sh results/
```
Fast overview of all files, identifies major issues.

### Step 2: Detailed Analysis (Python)
```bash
python3 stream_quality_analyzer.py results/ report.json summary.csv
```
Comprehensive analysis with exportable data.

### Step 3: Review Specific Files (OCaml or Python)
```bash
python3 stream_quality_analyzer.py results/problematic_file.sv
```
Deep dive into files that failed checks.

## 📈 Example Output

### Console Output
```
========================================
Quality Report: ariane_testharness.sv
========================================
Overall Score: 85.7%

📊 Individual Scores:
  ✓ Delimiter Balance: 100.0%
  ✓ Module Structure: 100.0%
  ⚠ No Artifacts: 71.4%
  ✓ Has Ports: 100.0%
  ✓ No Errors: 100.0%
  ✓ SV Features: 75.0%

📝 File Statistics:
  Lines: 1,247
  Characters: 45,892
  Max line length: 456

⚠️  Issues Found:
  • Debug variables: 12
```

### CSV Output Format
```csv
Filename,Overall Score,Delimiter Balance,Module Structure,...
ariane_testharness.sv,0.857,1.000,1.000,1.000,0.714,1.000,0.750,...
```

## 🔧 Integration with Your Workflow

### Adding to Makefile
```makefile
.PHONY: quality-check
quality-check:
	@echo "Running quality checks..."
	./compare_conversions.sh results/
	python3 stream_quality_analyzer.py results/ quality_report.json quality.csv
	@echo "Quality check complete!"
```

### CI/CD Integration
```bash
#!/bin/bash
# ci_quality_check.sh

python3 stream_quality_analyzer.py results/ report.json summary.csv

# Extract average score from JSON
avg_score=$(python3 -c "import json; print(json.load(open('report.json'))['summary']['avg_score'])")

# Fail if below threshold
if (( $(echo "$avg_score < 0.7" | bc -l) )); then
    echo "Quality check failed: $avg_score < 0.7"
    exit 1
fi

echo "Quality check passed: $avg_score"
```

## 📊 Tracking Quality Over Time

Create a tracking script:

```bash
#!/bin/bash
# track_quality.sh

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
OUTPUT_DIR="quality_history"
mkdir -p "$OUTPUT_DIR"

python3 stream_quality_analyzer.py results/ \
    "$OUTPUT_DIR/report_$TIMESTAMP.json" \
    "$OUTPUT_DIR/summary_$TIMESTAMP.csv"

# Append to history
echo "$TIMESTAMP,$(python3 -c "import json; print(json.load(open('$OUTPUT_DIR/report_$TIMESTAMP.json'))['summary']['avg_score'])")" \
    >> quality_history.csv
```

## 🐛 Troubleshooting

### Issue: "Verilator not found"
The bash script will skip Verilator checks. Install with:
```bash
# Ubuntu/Debian
sudo apt-get install verilator

# macOS
brew install verilator
```

### Issue: Python script too slow
The Python script is already memory-efficient, but you can:
1. Process fewer files at once
2. Use the bash script for quick checks
3. Run on specific problematic files only

### Issue: OCaml compilation errors
Ensure you have the required packages:
```bash
opam install yojson str
```

## 💡 Tips for Best Results

1. **Run after each major change** to catch regressions early
2. **Set quality thresholds** in your build system
3. **Track trends** over time to measure progress
4. **Focus on failed files** - don't waste time on passing files
5. **Use CSV exports** for data analysis in Excel/Python

## 📝 Customization

All tools are designed to be easily customizable:

- **Add new checks**: Modify the check functions
- **Adjust scoring**: Change score calculation logic
- **Add metrics**: Extend the QualityMetrics class
- **Change thresholds**: Update pass/fail criteria

## 🤝 Contributing

To add a new quality check:

1. Add metric tracking in appropriate tool
2. Implement scoring logic
3. Update report generation
4. Document in this README

---

**Need help?** Check the inline comments in each tool for detailed explanations.
