#
# Makefile
#
all: test_eval test_invert test_print

test_eval: eval_test.byte
	./eval_test.byte

eval_test.byte: 		eval_test.ml
	(cd ../src; make)
	ocamlfind ocamlc -o $@ -package oUnit -linkpkg -g -I ../src util.cmo invert.cmo print.cmo eval.cmo value.cmo syntax.cmo eval_test.ml

test_invert: invert_test.byte
	./invert_test.byte

invert_test.byte: 		invert_test.ml
	(cd ../src; make)
	ocamlfind ocamlc -o $@ -package oUnit -linkpkg -g -I ../src invert.cmo syntax.cmo invert_test.ml

test_print: print_test.byte
	./print_test.byte

print_test.byte: 		print_test.ml
	(cd ../src; make)
	ocamlfind ocamlc -o $@ -package oUnit -linkpkg -g -I ../src value.cmo print.cmo print_test.ml

clean:
	/bin/rm -f *.byte
	/bin/rm -f *.cmi *.cmo *.mli

veryclean: clean
	/bin/rm -f *.log *.cache
