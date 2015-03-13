all:
	ocamllex lexer.mll
	ocamlc -c auxparser.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c pascaml.ml
	ocamlc -o pascaml auxparser.cmo lexer.cmo parser.cmo pascaml.cmo

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli pascaml

