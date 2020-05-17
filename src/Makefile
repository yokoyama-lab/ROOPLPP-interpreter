#
# Makefile
#

SRC= syntax.ml value.ml inver.ml eval.ml print.ml parser.mly lexer.mll main.ml
COMPONENT= syntax.ml value.ml invert.ml eval.ml print.ml parser.mli parser.ml lexer.ml main.ml
TARGET= test

all:	$(TARGET)

$(TARGET): 	$(COMPONENT)
	ocamlmktop $(COMPONENT) -w -31 -o $(TARGET)

parser.mli:	parser.mly
	ocamlyacc parser.mly

parser.ml:	parser.mly
	ocamlyacc parser.mly

lexer.ml:	lexer.mll
	ocamllex lexer.mll

backup:
	/bin/cp -f Makefile $(SRC) back

clean:
	/bin/rm -f parser.ml parser.mli lexer.ml $(TARGET) *.cmi *.cmo *.mli parser.conflicts parser.output
