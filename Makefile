TAGS:
	find . -name '*.hs' | xargs hasktags -e -o - > TAGS

rebuild:
	cabal-delete gitlib-s3
	cabal-delete gitlib
	cabal-delete hlibgit2
	cabal-delete aws
	cabal-meta install -j1
