MODULES=hogwarts command state engine authors
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
<<<<<<< HEAD
	ocamlbuild -clean
=======
	ocamlbuild -clean
	
>>>>>>> master
