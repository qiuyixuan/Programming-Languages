
ocamlyacc -q parser.mly
ocamllex -q lexer.mll

ocamlc -g -c ast.ml
ocamlc -g -c parser.mli
ocamlc -g -c parser.ml
ocamlc -g -c lexer.ml
ocamlc -g -c server.ml
ocamlc -g -c pprint.ml
ocamlc -g -c eval.ml
ocamlc -g -c translate.ml
ocamlc -g -c main.ml

ocamlc -g -o cps server.cmo ast.cmo lexer.cmo parser.cmo pprint.cmo eval.cmo translate.cmo main.cmo


