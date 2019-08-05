all:
	dune build @install

doc:
	dune build @doc

clean:
	dune clean

pushdoc: doc
	git checkout gh-pages || git checkout --orphan gh-pages
	cp -r _build/default/_doc/_html/* . && git add .
	git commit --allow-empty -am "update documentation"
	git push
	git checkout master

.PHONY: all doc clean pushdoc
