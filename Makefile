default: build

build:
	ocamlbuild -use-ocamlfind -pkgs opal src/lisp_test.native

clean:
	rm -rf _build

test: build
	./lisp_test.native
