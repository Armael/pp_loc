all:
	dune build @install

doc:
	dune build @doc

clean:
	dune clean

.PHONY: all doc clean
