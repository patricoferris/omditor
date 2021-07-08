all:
				dune build --profile release src/client/index.bc.js
				dune exec --profile release -- ./src/server/main.exe

doc:
				dune runtest -p omditor --auto-promote

fmt: 
				dune build @fmt --auto