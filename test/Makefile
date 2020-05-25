#
# Makefile
#

all: test_exp test_state

test_exp: eval_exp_test.byte
	./eval_exp_test.byte

test_state: eval_state_test.byte
	./eval_state_test.byte

eval_exp_test.byte: 		eval_exp_test.ml
	(cd ../src; make)
	ocamlfind ocamlc -o $@ -package ounit2 -linkpkg -g -I ../src invert.cmo eval.cmo value.cmo syntax.cmo eval_exp_test.ml

eval_state_test.byte:		eval_state_test.ml
	(cd ../src; make)
	ocamlfind ocamlc -o $@ -package ounit2 -linkpkg -g -I ../src invert.cmo eval.cmo eval_state_test.ml

clean:
	/bin/rm -f *.byte
	/bin/rm -f *.cmi *.cmo *.mli

veryclean: clean
	/bin/rm -f *.log *.cache
