# Makefile for Verilog Decompiler and Validation Suite

# OCaml compiler settings
OCAMLFIND = ocamlfind
OCAMLC = $(OCAMLFIND) ocamlc -I ..
OCAMLOPT = $(OCAMLFIND) ocamlopt
PACKAGES = -package yojson,unix,str

# Source files
SOURCES = ../sv_ast.mli ../sv_parse.ml ../sv_gen.ml ../sv_main.ml sv_args.ml
TEST_SOURCES = test_decompiler.ml
VALIDATOR_SOURCES = roundtrip_validator.ml
QUALITY_SOURCES = quality_checker.ml

# Output binaries
DECOMPILER = sv_main
TEST_RUNNER = test_decompiler
VALIDATOR = roundtrip_validator
QUALITY_CHECKER = quality_checker

# Directories
OBJ_DIR = obj_dir
RESULTS_DIR = results
ROUNDTRIP_DIR = roundtrip_json
REPORTS_DIR = validation_reports

.PHONY: all clean decompiler test validator quality-checker run-decompiler \
        roundtrip quality validate-all help

# Default target
all: decompiler validator quality-checker
	@echo ""
	@echo "✓ All tools compiled successfully!"
	@echo ""
	@echo "Next steps:"
	@echo "  make run-decompiler  - Run the decompiler"
	@echo "  make validate-all    - Run complete validation suite"
	@echo "  make help            - Show all available targets"

# Compile the main decompiler
decompiler: $(SOURCES)
	@echo "Compiling decompiler..."
	$(OCAMLC) $(PACKAGES) -linkpkg $(SOURCES) -o $(DECOMPILER)
	@echo "✓ Decompiler compiled: $(DECOMPILER)"

# Compile test runner
test: $(SOURCES) $(TEST_SOURCES)
	@echo "Compiling test runner..."
	$(OCAMLC) $(PACKAGES) -linkpkg $(SOURCES) $(TEST_SOURCES) -o $(TEST_RUNNER)
	@echo "✓ Test runner compiled: $(TEST_RUNNER)"
	@echo "Running tests..."
	./$(TEST_RUNNER)

# Compile round-trip validator
validator: $(SOURCES) $(VALIDATOR_SOURCES)
	@echo "Compiling round-trip validator..."
	$(OCAMLC) $(PACKAGES) -linkpkg $(SOURCES) $(VALIDATOR_SOURCES) -o $(VALIDATOR)
	@echo "✓ Validator compiled: $(VALIDATOR)"

# Compile quality checker
quality-checker: $(QUALITY_SOURCES)
	@echo "Compiling quality checker..."
	$(OCAMLC) $(PACKAGES) -linkpkg $(QUALITY_SOURCES) -o $(QUALITY_CHECKER)
	@echo "✓ Quality checker compiled: $(QUALITY_CHECKER)"

# Run the decompiler
run-decompiler: decompiler
	@echo "Running decompiler on $(OBJ_DIR)/*.json..."
	@mkdir -p $(RESULTS_DIR)
	./$(DECOMPILER)
	@echo "✓ Decompilation complete. Output in: $(RESULTS_DIR)/"

# Run quality assessment
quality: quality-checker
	@echo "Running quality assessment..."
	@mkdir -p $(REPORTS_DIR)
	@if command -v python3 >/dev/null 2>&1; then \
		echo "Using Python stream analyzer (recommended)..."; \
		python3 stream_quality_analyzer.py $(RESULTS_DIR) \
			$(REPORTS_DIR)/quality_latest.json \
			$(REPORTS_DIR)/quality_latest.csv; \
	else \
		echo "Using OCaml quality checker..."; \
		./$(QUALITY_CHECKER) $(RESULTS_DIR) $(REPORTS_DIR)/quality_latest.csv; \
	fi

# Run round-trip validation
roundtrip: validator run-decompiler
	@echo "Running round-trip validation..."
	@chmod +x run_roundtrip_test.sh
	./run_roundtrip_test.sh $(OBJ_DIR)

# Run complete validation suite
validate-all: all run-decompiler
	@echo "Running complete validation suite..."
	@chmod +x validate_all.sh
	./validate_all.sh

# Quick validation (quality check only)
quick-check: decompiler quality
	@echo "Quick validation complete!"

# Clean build artifacts
clean:
	rm -f $(DECOMPILER) $(TEST_RUNNER) $(VALIDATOR) $(QUALITY_CHECKER)
	rm -f *.cmi *.cmo *.cmx *.o
	rm -rf _build
	@echo "✓ Build artifacts cleaned"

# Clean all generated files
clean-all: clean
	rm -rf $(RESULTS_DIR)
	rm -rf $(ROUNDTRIP_DIR)
	rm -rf $(ROUNDTRIP_JSON_DIR)
	rm -rf $(REPORTS_DIR)
	@echo "✓ All generated files cleaned"

# Setup: make scripts executable
setup:
	chmod +x run_roundtrip_test.sh
	chmod +x validate_all.sh
	chmod +x compare_conversions.sh
	chmod +x stream_quality_analyzer.py
	chmod +x ast_diff_viewer.py
	@echo "✓ Scripts made executable"

# Check dependencies
check-deps:
	@echo "Checking dependencies..."
	@command -v ocaml >/dev/null 2>&1 || (echo "✗ OCaml not found"; exit 1)
	@command -v ocamlfind >/dev/null 2>&1 || (echo "✗ ocamlfind not found"; exit 1)
	@echo "  ✓ OCaml found: $$(ocaml -version)"
	@echo "  ✓ ocamlfind found"
	@$(OCAMLFIND) query yojson >/dev/null 2>&1 && echo "  ✓ yojson package found" || \
		(echo "  ✗ yojson not found. Install with: opam install yojson"; exit 1)
	@command -v python3 >/dev/null 2>&1 && echo "  ✓ Python 3 found" || echo "  ⚠ Python 3 not found (optional)"
	@command -v verilator >/dev/null 2>&1 && echo "  ✓ Verilator found" || \
		echo "  ⚠ Verilator not found (required for round-trip testing)"
	@echo "✓ Dependency check complete"

# Install OCaml dependencies
install-deps:
	@echo "Installing OCaml dependencies..."
	opam install yojson
	@echo "✓ Dependencies installed"

# Help target
help:
	@echo "Verilog Decompiler - Makefile Help"
	@echo ""
	@echo "Building:"
	@echo "  make                 - Build all tools (default)"
	@echo "  make decompiler      - Build only the decompiler"
	@echo "  make validator       - Build round-trip validator"
	@echo "  make quality-checker - Build quality checker"
	@echo "  make test            - Build and run unit tests"
	@echo ""
	@echo "Running:"
	@echo "  make run-decompiler  - Run decompiler on obj_dir/*.json"
	@echo "  make quality         - Run quality assessment"
	@echo "  make roundtrip       - Run round-trip validation"
	@echo "  make validate-all    - Run complete validation suite"
	@echo "  make quick-check     - Quick quality check only"
	@echo ""
	@echo "Maintenance:"
	@echo "  make clean           - Remove build artifacts"
	@echo "  make clean-all       - Remove all generated files"
	@echo "  make setup           - Make scripts executable"
	@echo "  make check-deps      - Check dependencies"
	@echo "  make install-deps    - Install OCaml dependencies"
	@echo ""
	@echo "Directories:"
	@echo "  $(OBJ_DIR)        - Input JSON files from Verilator"
	@echo "  $(RESULTS_DIR)    - Decompiled SystemVerilog output"
	@echo "  $(ROUNDTRIP_DIR)  - Round-trip JSON files"
	@echo "  $(REPORTS_DIR)    - Validation reports"

# Pretty output target
.PHONY: status
status:
	@echo "Project Status"
	@echo "=============="
	@echo ""
	@echo "Binaries:"
	@test -f $(DECOMPILER) && echo "  ✓ $(DECOMPILER)" || echo "  ✗ $(DECOMPILER) (not built)"
	@test -f $(VALIDATOR) && echo "  ✓ $(VALIDATOR)" || echo "  ✗ $(VALIDATOR) (not built)"
	@test -f $(QUALITY_CHECKER) && echo "  ✓ $(QUALITY_CHECKER)" || echo "  ✗ $(QUALITY_CHECKER) (not built)"
	@echo ""
	@echo "Directories:"
	@test -d $(OBJ_DIR) && echo "  ✓ $(OBJ_DIR) ($$(ls -1 $(OBJ_DIR)/*.json 2>/dev/null | wc -l) JSON files)" || echo "  ✗ $(OBJ_DIR) (not found)"
	@test -d $(RESULTS_DIR) && echo "  ✓ $(RESULTS_DIR) ($$(ls -1 $(RESULTS_DIR)/*.sv 2>/dev/null | wc -l) SV files)" || echo "  ○ $(RESULTS_DIR) (empty)"
	@test -d $(REPORTS_DIR) && echo "  ✓ $(REPORTS_DIR)" || echo "  ○ $(REPORTS_DIR) (not created yet)"
