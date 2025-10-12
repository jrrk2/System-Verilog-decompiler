#!/bin/bash
# compare_conversions.sh - Compare conversion quality across multiple runs

set -e

RESULTS_DIR=${1:-"results"}
METRICS_FILE="conversion_metrics.txt"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

echo "==================================="
echo "Verilog Conversion Quality Checker"
echo "==================================="
echo "Analyzing directory: $RESULTS_DIR"
echo ""

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Initialize counters
total_files=0
passed_files=0
failed_files=0

# Create metrics file
echo "Conversion Metrics - $TIMESTAMP" > "$METRICS_FILE"
echo "=================================" >> "$METRICS_FILE"
echo "" >> "$METRICS_FILE"

# Function to check if verilator can parse the file
check_verilator() {
    local file="$1"
    if command -v verilator &> /dev/null; then
        if verilator --lint-only -Wall "$file" 2>&1 | grep -q "Error"; then
            return 1
        else
            return 0
        fi
    else
        echo "  (Verilator not available, skipping syntax check)"
        return 2
    fi
}

# Function to analyze a single file
analyze_file() {
    local file="$1"
    local basename=$(basename "$file")
    
    echo "Checking: $basename"
    
    # Count lines
    local lines=$(wc -l < "$file")
    
    # Check for basic structure
    local has_module=$(grep -c "^module " "$file" || true)
    local has_endmodule=$(grep -c "^endmodule" "$file" || true)
    
    # Check for potential issues
    local empty_blocks=$(grep -c "begin.*end" "$file" | grep -v "//" || true)
    local debug_vars=$(grep -c "__Vdfg" "$file" || true)
    local unknown_types=$(grep -c "UNKNOWN" "$file" || true)
    local json_fragments=$(grep -c '"type":' "$file" || true)
    
    # Count port declarations
    local inputs=$(grep -c "^  input " "$file" || true)
    local outputs=$(grep -c "^  output " "$file" || true)
    
    # Check balanced delimiters
    local open_parens=$(grep -o "(" "$file" | wc -l)
    local close_parens=$(grep -o ")" "$file" | wc -l)
    local open_braces=$(grep -o "{" "$file" | wc -l)
    local close_braces=$(grep -o "}" "$file" | wc -l)
    
    local issues=0
    local warnings=0
    
    # Evaluate issues
    if [ $has_module -eq 0 ] || [ $has_endmodule -eq 0 ]; then
        echo -e "  ${RED}✗ Missing module structure${NC}"
        issues=$((issues + 1))
    fi
    
    if [ $open_parens -ne $close_parens ]; then
        echo -e "  ${RED}✗ Unbalanced parentheses: $open_parens open, $close_parens close${NC}"
        issues=$((issues + 1))
    fi
    
    if [ $open_braces -ne $close_braces ]; then
        echo -e "  ${RED}✗ Unbalanced braces: $open_braces open, $close_braces close${NC}"
        issues=$((issues + 1))
    fi
    
    if [ $debug_vars -gt 0 ]; then
        echo -e "  ${YELLOW}⚠ Debug variables found: $debug_vars${NC}"
        warnings=$((warnings + 1))
    fi
    
    if [ $unknown_types -gt 0 ]; then
        echo -e "  ${YELLOW}⚠ Unknown types: $unknown_types${NC}"
        warnings=$((warnings + 1))
    fi
    
    if [ $json_fragments -gt 0 ]; then
        echo -e "  ${RED}✗ JSON fragments remaining: $json_fragments${NC}"
        issues=$((issues + 1))
    fi
    
    # Try verilator if available
    check_verilator "$file"
    local verilator_result=$?
    if [ $verilator_result -eq 0 ]; then
        echo -e "  ${GREEN}✓ Verilator syntax check passed${NC}"
    elif [ $verilator_result -eq 1 ]; then
        echo -e "  ${RED}✗ Verilator syntax check failed${NC}"
        issues=$((issues + 1))
    fi
    
    # Summary for this file
    echo "  Lines: $lines, Inputs: $inputs, Outputs: $outputs"
    
    # Write to metrics file
    echo "File: $basename" >> "$METRICS_FILE"
    echo "  Lines: $lines" >> "$METRICS_FILE"
    echo "  Ports: $inputs inputs, $outputs outputs" >> "$METRICS_FILE"
    echo "  Issues: $issues, Warnings: $warnings" >> "$METRICS_FILE"
    echo "  Debug vars: $debug_vars, Unknown types: $unknown_types" >> "$METRICS_FILE"
    echo "" >> "$METRICS_FILE"
    
    if [ $issues -eq 0 ]; then
        echo -e "  ${GREEN}✓ PASSED${NC}"
        passed_files=$((passed_files + 1))
    else
        echo -e "  ${RED}✗ FAILED${NC}"
        failed_files=$((failed_files + 1))
    fi
    
    echo ""
}

# Find all .sv files
echo "Scanning for SystemVerilog files..."
sv_files=$(find "$RESULTS_DIR" -name "*.sv" -type f)

if [ -z "$sv_files" ]; then
    echo "No .sv files found in $RESULTS_DIR"
    exit 1
fi

# Count total files
total_files=$(echo "$sv_files" | wc -l)

echo "Found $total_files files to analyze"
echo ""

# Analyze each file
while IFS= read -r file; do
    analyze_file "$file"
done <<< "$sv_files"

# Print final summary
echo "==================================="
echo "Summary"
echo "==================================="
echo "Total files: $total_files"
echo -e "${GREEN}Passed: $passed_files${NC}"
echo -e "${RED}Failed: $failed_files${NC}"

success_rate=$(awk "BEGIN {printf \"%.1f\", ($passed_files / $total_files) * 100}")
echo "Success rate: $success_rate%"

# Write summary to metrics file
echo "==================================" >> "$METRICS_FILE"
echo "Summary" >> "$METRICS_FILE"
echo "==================================" >> "$METRICS_FILE"
echo "Total files: $total_files" >> "$METRICS_FILE"
echo "Passed: $passed_files" >> "$METRICS_FILE"
echo "Failed: $failed_files" >> "$METRICS_FILE"
echo "Success rate: $success_rate%" >> "$METRICS_FILE"

echo ""
echo "Detailed metrics saved to: $METRICS_FILE"
