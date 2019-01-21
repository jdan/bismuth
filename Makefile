default: build

build:
	ocamlbuild -use-ocamlfind -pkgs opal src/lang.native

build-test:
	ocamlbuild -use-ocamlfind -pkgs opal src/test.native

clean:
	rm -rf _build

test: build-test
	./test.native
