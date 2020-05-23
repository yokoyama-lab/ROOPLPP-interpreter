#
# Makefile
#

test_exp:  		eval_exp_test.ml
	ocamlbuild -pkgs oUnit eval_exp_test.byte
	./eval_exp_test.byte

test_state:		eval_state_test.ml
	ocamlbuild -pkgs oUnit eval_state_test.byte
	./eval_state_test.byte
clean:
	/bin/rm -f *.byte
