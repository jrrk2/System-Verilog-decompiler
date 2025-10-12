# Quick Reference Guide - Decompiler Validation

## 🚀 One-Command Validation

```bash
# Run everything at once
./validate_all.sh
```

This will:
- ✅ Check output quality
- ✅ Run round-trip validation
- ✅ Generate all reports
- ✅ Create HTML dashboard

---

## 📋 Individual Tools

### Quality Check Only
```bash
# Fast - checks syntax, structure, common errors
python3 stream_quality_analyzer.py results/ quality.json quality.csv
```

### Round-Trip Test Only
```bash
# Full - compares AST trees
./run_roundtrip_test.sh obj_dir/
```

### Detailed File Analysis
```bash
# Deep dive into one file
python3 ast_diff_viewer.py \
    obj_dir/module.json \
    roundtrip_json/module.json \
    --tree
```

---

## 🎯 Common Workflows

### Testing After Code Changes
```bash
# Quick quality check
python3 stream_quality_analyzer.py results/

# If quality looks good, run full round-trip
./run_roundtrip_test.sh obj_dir/
```

### Investigating a Failing File
```bash
# 1. Get detailed comparison
python3 ast_diff_viewer.py \
    obj_dir/failing_module.json \
    roundtrip_json/failing_module.json \
    --tree

# 2. Check the decompiled SV directly
cat results/decompile_failing_module.sv

# 3. Try recompiling manually
verilator --lint-only results/decompile_failing_module.sv
```

### Tracking Quality Over Time
```bash
# Run and save with timestamp
./validate_all.sh

# Results saved to validation_reports/
# Compare with previous runs
```

---

## 📊 Interpreting Scores

### Quality Score
- **≥90%** - Excellent ✅
- **80-89%** - Good ⚠️
- **70-79%** - Fair ⚠️
- **<70%** - Poor ❌

### Round-Trip Similarity
- **100%** - Perfect match! 🎉
- **95-99%** - Minor differences ✅
- **85-94%** - Acceptable ⚠️
- **<85%** - Significant loss ❌

---

## 🔍 Finding Specific Issues

### Show Files with Most Errors
```bash
# From quality report
tail -n +2 quality.csv | sort -t',' -k10 -nr | head -10

# From round-trip report
tail -n +2 roundtrip_report.csv | sort -t',' -k4 -nr | head -10
```

### Find Files Below Threshold
```bash
# Quality < 80%
awk -F',' 'NR>1 && $2<0.8 {print $1, $2*100"%"}' quality.csv

# Similarity < 90%
awk -F',' 'NR>1 && $3<0.9 {print $1, $3*100"%"}' roundtrip_report.csv
```

### Check Specific Error Types
```bash
# Files with unbalanced delimiters
grep "false" quality.csv | grep "Delimiter"

# Files with module mismatches
grep "false" roundtrip_report.csv | awk -F',' '$5>0 {print $1}'
```

---

## 🛠️ Troubleshooting

### "No .sv files found"
```bash
# Check if decompiler ran
ls -la results/

# Run decompiler manually
./decompiler  # or ./sv_main
```

### "Verilator not found"
```bash
# Install Verilator
sudo apt-get install verilator  # Ubuntu/Debian
brew install verilator          # macOS
```

### "Compilation failed"
```bash
# Check specific file
verilator --lint-only results/decompile_module.sv

# Common fixes:
# - Check for syntax errors
# - Verify module names
# - Check port declarations
```

### "Python module not found"
```bash
# Install required packages
pip3 install --user json pathlib
```

---

## 📝 File Locations

```
project/
├── obj_dir/              # Original JSON files (input)
├── results/              # Decompiled .sv files
├── roundtrip_json/       # Re-compiled JSON files
└── validation_reports/   # All validation reports
    ├── summary_*.txt
    ├── report_*.html
    ├── quality_*.csv
    └── roundtrip_*.csv
```

---

## ⚡ Performance Tips

### For Large Projects
```bash
# Test subset first
./run_roundtrip_test.sh obj_dir/ --max-files 10

# Or run in parallel
find results/ -name "*.sv" | parallel -j4 verilator --lint-only {}
```

### Memory Constrained
```bash
# Use streaming analyzer (memory efficient)
python3 stream_quality_analyzer.py results/

# Process one file at a time
for f in results/*.sv; do
    python3 stream_quality_analyzer.py "$f"
done
```

---

## 🎯 Setting Up CI/CD

### Basic Pipeline
```yaml
test:
  script:
    - ./validate_all.sh
    - |
      if [ $? -ne 0 ]; then
        echo "Validation failed"
        exit 1
      fi
```

### With Thresholds
```bash
#!/bin/bash
# ci_check.sh

./validate_all.sh

QUALITY=$(python3 -c "import json; print(json.load(open('validation_reports/quality_latest.json'))['summary']['avg_score'])")

if (( $(echo "$QUALITY < 0.85" | bc -l) )); then
    echo "Quality too low: $QUALITY"
    exit 1
fi
```

---

## 📈 Continuous Improvement

### Weekly Quality Review
1. Run `./validate_all.sh`
2. Review HTML report
3. Pick top 5 failing files
4. Fix issues
5. Re-run validation
6. Track improvement

### Monthly Analysis
1. Compare current vs previous month
2. Identify trends
3. Update test cases
4. Document known issues

---

## 🆘 Quick Help

```bash
# Any tool help
./run_roundtrip_test.sh --help
python3 stream_quality_analyzer.py --help
python3 ast_diff_viewer.py --help

# View this guide
cat QUICK_REFERENCE.md
```

---

## 📚 Full Documentation

- **Quality Tools**: `QUALITY_TOOLS_README.md`
- **Round-Trip Testing**: `ROUNDTRIP_TESTING_README.md`
- **Main README**: `README.md`
