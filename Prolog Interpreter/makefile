make:
	ocamlc -c abstract_types.ml
	ocamlyacc -v parser.mly 
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c calc.ml
	ocamlc -c main.ml
	ocamlc -o main lexer.cmo parser.cmo abstract_types.cmo calc.cmo main.cmo
	./main