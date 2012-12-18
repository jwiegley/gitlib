To install the libgit package run as root:
    build.sh

run cabal
    cabal configure --enable-test
    cabal build
    cabal test
    cabal install
