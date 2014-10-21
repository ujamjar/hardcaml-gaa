all: lib

lib:
	ocamlbuild -use-ocamlfind Gaa.cma

dsl: lib
	ocamlbuild -use-ocamlfind dsl.byte

clean:
	ocamlbuild -clean
	-rm -f dsl.byte
	-find . -name "*~" | xargs rm -f

