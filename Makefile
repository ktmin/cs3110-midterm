MODULES=hogwarts command state authors model controller view ai_controller
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=controller.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=yojson,ANSITerminal,str,qcheck

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)
 
test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)
