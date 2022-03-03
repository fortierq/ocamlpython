
all: build
	dune exec ./minipython.exe test.py

tests: build
	bash run-tests "dune exec ./minipython.exe"

build:
	dune build minipython.exe

clean:
	dune clean

.PHONY: all clean minipython.exe
