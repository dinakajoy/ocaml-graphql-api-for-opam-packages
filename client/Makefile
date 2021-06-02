install:
	opam install js_of_ocaml js_of_ocaml-compiler js_of_ocaml-ppx ezjsonm

build:
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml-ppx -package js_of_ocaml-lwt -package ezjsonm -package ezjsonm -linkpkg -o ./dist/js/main.byte ./src/main.ml

com:
	js_of_ocaml ./dist/js/main.byte
