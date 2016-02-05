.PHONY: default

default: clean tests.native

tests.native:
	eval $$(opam config env) ocamlbuild -use-ocamlfind tests.native

test: tests.native
	python test.py

test-oopsla: tests.native
	python test.py oopsla

clean:
	ocamlbuild -clean
