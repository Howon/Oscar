#! /bin/bash

ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.ml
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -c prettyprint.ml
ocamlc -o prettyprint ast.cmo parser.cmo scanner.cmo prettyprint.cmo
