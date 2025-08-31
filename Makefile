# Alternative Makefile for manual building
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLMKTOP = ocamlmktop
MENHIR = menhir
OCAMLLEX = ocamllex

SOURCES = sv_ast.mli sv_parse.ml sv_gen.ml sv_main.ml

TARGET = json_verilog
TARGET_TOP = json_verilog_top

.PHONY: all clean debug

all: $(TARGET) $(TARGET_TOP)

debug: OCAML_FLAGS += -g
debug: $(TARGET)

json_verilog.ml json_verilog.mli: json_verilog.mly json_types.cmi
	$(MENHIR) --explain --dump --infer $<

json_verilog_lexer.ml: json_verilog_lexer.mll json_verilog.mli
	$(OCAMLLEX) $<

json_types.cmi: json_types.mli
	$(OCAMLC) -c $<

$(TARGET_TOP): $(SOURCES)
	ocamlfind $(OCAMLMKTOP) -package yojson,str,unix -linkpkg -I +unix -o $@ $^

$(TARGET): $(SOURCES)
	ocamlfind $(OCAMLOPT) -package yojson,str,unix -linkpkg -I +unix -o $@ $^

clean:
	rm -f *.cmi *.cmx *.cmo *.o $(TARGET)
	rm -f json_verilog.ml json_verilog.mli json_verilog_lexer.ml
	rm -f json_verilog.automaton json_verilog.conflicts

install: $(TARGET)
	cp $(TARGET) /usr/local/bin/

test: $(TARGET)
	./$(TARGET) -da test.json test.v

.INTERMEDIATE: json_verilog.ml json_verilog.mli json_verilog_lexer.ml
