default: build

build:
	ocamlbuild src/main_test.native

clean:
	rm -rf _build

test: build
	./main_test.native