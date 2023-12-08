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
	rm -f 3110-final-project.zip
	zip -r 3110-final-project.zip ../

clean:
	dune clean
	rm -f word_hex.zip

doc:
	dune build @doc