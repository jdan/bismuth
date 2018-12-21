default: build

build:
	ocamlbuild src/lisp_test.native

clean:
	rm -rf _build

test: build
	./lisp_test.native