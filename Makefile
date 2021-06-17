install:
	opam install opampackages-server --deps-only
	opam install opampackages-client --deps-only

build:
	dune build

run-client:
	rm -rf dist/ui/js
	mkdir -p dist/ui/js
	cp _build/default/client/main.js dist/ui/js/main.js
	open-cli ./dist/index.html --google chrome

run-server:
	rm -rf dist/server
	mkdir -p dist/server
	cp _build/default/server/app.exe dist/server/app.exe
	dune exec ./dist/server/app.exe
