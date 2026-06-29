#
# Makefile
#
all: test_eval test_invert test_print test_pretty test_env test_eval_prog

# Build the interpreter objects in ../src exactly once per make run.
# Used as an order-only prerequisite (after the | in each *.byte rule) so the
# src build is shared by all suites instead of being re-invoked once per suite,
# while a byte still only relinks when its own _test.ml changes.
.PHONY: build-src
build-src:
	(cd ../src; make)

test_eval: eval_test.byte
	./eval_test.byte

eval_test.byte: 		eval_test.ml | build-src
	ocamlfind ocamlc -o $@ -package ounit2 -linkpkg -g -I ../src util.cmo pretty.cmo invert.cmo print.cmo eval.cmo value.cmo syntax.cmo eval_test.ml

test_invert: invert_test.byte
	./invert_test.byte

invert_test.byte: 		invert_test.ml | build-src
	ocamlfind ocamlc -o $@ -package ounit2 -linkpkg -g -I ../src invert.cmo syntax.cmo invert_test.ml

test_print: print_test.byte
	./print_test.byte

print_test.byte: 		print_test.ml | build-src
	ocamlfind ocamlc -o $@ -package ounit2 -linkpkg -g -I ../src value.cmo print.cmo print_test.ml

test_pretty: pretty_test.byte
	./pretty_test.byte

pretty_test.byte: 		pretty_test.ml | build-src
	ocamlfind ocamlc -o $@ -package ounit2 -linkpkg -g -I ../src util.cmo syntax.cmo value.cmo pretty.cmo pretty_test.ml

test_env: env_store_test.byte
	./env_store_test.byte

env_store_test.byte: 		env_store_test.ml | build-src
	ocamlfind ocamlc -o $@ -package ounit2 -linkpkg -g -I ../src util.cmo pretty.cmo invert.cmo print.cmo value.cmo syntax.cmo eval.cmo env_store_test.ml

test_eval_prog: eval_prog_test.byte
	./eval_prog_test.byte

eval_prog_test.byte: 		eval_prog_test.ml | build-src
	ocamlfind ocamlc -o $@ -package ounit2 -linkpkg -g -I ../src util.cmo syntax.cmo value.cmo pretty.cmo invert.cmo print.cmo eval.cmo parser.cmo lexer.cmo eval_prog_test.ml

clean:
	$(RM) *.byte
	$(RM) *.cmi *.cmo *.mli

veryclean: clean
	$(RM) *.log *.cache
