.PHONY: docs

BUILD=ocamlbuild -use-ocamlfind -pkgs opal

default: build

build:
	$(BUILD) src/lang.native

build-test:
	$(BUILD) src/test.native

docs:
	rm -rf docs
	$(BUILD) src/lang.docdir/index.html
	mv _build/src/lang.docdir docs
	rm lang.docdir

clean:
	rm -rf _build

test: build-test
	./test.native
