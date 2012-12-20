#!/bin/sh

cabal-delete gitlib-s3
cabal-delete gitlib
cabal-delete hlibgit2
cabal-meta install -j --only-dependencies
cabal-meta install -j1

exit 0
