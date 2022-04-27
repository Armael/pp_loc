all:
	dune build @install

doc:
	dune build @doc

clean:
	dune clean

WATCH?=@install
watch:
	dune build $(WATCH) -w

pushdoc:
	git stash
	dune build @doc
	git checkout gh-pages || git checkout --orphan gh-pages
	cp -r _build/default/_doc/_html/* . && git add .
	git commit --allow-empty -am "update documentation" && git push
	git checkout master && git stash pop

.PHONY: all doc clean pushdoc
