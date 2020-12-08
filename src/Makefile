#
# Makefile
#

SRC= util.ml syntax.ml value.ml pretty.ml invert.ml print.ml eval.ml parser.mly lexer.mll main.ml
COMPONENT= util.ml syntax.ml value.ml pretty.ml invert.ml print.ml eval.ml parser.mli parser.ml lexer.ml main.ml
TARGET= rplpp
PACKAGE	= ROOPLPP
FILES	= Makefile $(SRC)
VER	= `date +%Y%m%d`

all:	$(TARGET)

$(TARGET): 	$(COMPONENT)
	ocamlc $(COMPONENT) -o $(TARGET)

parser.mli:	parser.mly
	ocamlyacc parser.mly

parser.ml:	parser.mly
	ocamlyacc parser.mly

lexer.ml:	lexer.mll
	ocamllex lexer.mll

tar:
	@echo $(PACKAGE)-$(VER) > .package
	@$(RM) -r `cat .package`
	@mkdir `cat .package`
	@ln $(FILES) `cat .package`
	tar cvf - `cat .package` | gzip -9 > `cat .package`.tar.gz
	@$(RM) -r `cat .package` .package

zip:
	zip -9 $(PACKAGE)-$(VER).zip $(FILES)

clean:
	$(RM) parser.ml parser.mli lexer.ml $(TARGET) *.cmi *.cmo *.mli parser.conflicts parser.output

test:	$(TARGET)
	./$(TARGET) ../example/fib.rplpp
	./$(TARGET) ../example/factor.rplpp
	./$(TARGET) ../example/sqrt.rplpp
	./$(TARGET) ../example/perm-to-code.rplpp
	./$(TARGET) ../example/LinkedList.rplpp
	./$(TARGET) ../example/LinkedList_print.rplpp
	./$(TARGET) ../example/BinaryTree.rplpp
	./$(TARGET) ../example/BinaryTree_print.rplpp
	./$(TARGET) ../example/DoublyLinkedList.rplpp
	./$(TARGET) ../example/DoublyLinkedList_print.rplpp
	./$(TARGET) ../example/for.rplpp
	./$(TARGET) ../example/switch.rplpp
	./$(TARGET) ../example/instanceSwap.rplpp
	./$(TARGET) ../example/accessInstance.rplpp
	./$(TARGET) ../example/assignInstance.rplpp
