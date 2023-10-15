.PHONY: test

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

word_hex:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f word_hex.zip
	zip -r word_hex.zip . -x@exclude.lst

clean:
	dune clean
	rm -f word_hex.zip
