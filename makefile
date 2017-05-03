all: miniml test
test : tester tests t

miniml: miniml.ml
	ocamlbuild miniml.byte

run: 
	./miniml.byte

t: 
	./tests.byte

tester: tester.ml 
	ocamlbuild -lib unix tester.byte

tests: tests.ml
	ocamlbuild -lib unix tests.byte


clean: 	
	rm -rf _build *.byte 