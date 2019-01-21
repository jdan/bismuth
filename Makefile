default: build

build:
	ocamlbuild -use-ocamlfind -pkgs opal src/lang.native

build-test:
	ocamlbuild -use-ocamlfind -pkgs opal src/lang_test.native

clean:
	rm -rf _build

test: build-test
	./lang_test.native
