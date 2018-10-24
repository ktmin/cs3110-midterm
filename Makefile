MODULES=hogwarts command state engine authors
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=engine.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean
	

play:
		$(OCAMLBUILD) $(MAIN) && ./$(MAIN)
