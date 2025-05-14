install:
	opam install . --deps-only

build:
	dune build

run-client:
	rm -rf dist/ui/js
	mkdir -p dist/ui/js
	cp _build/default/client/main.bc.js dist/ui/js/main.js
	xdg-open ./dist/ui/index.html

run-server:
	rm -rf dist/server
	mkdir -p dist/server
	cp _build/default/server/app.exe dist/server/app.exe
	dune exec ./dist/server/app.exe
