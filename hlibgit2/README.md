To install the libgit package run as root:
    build.sh

run cabal
    cabal configure --enable-test
    cabal build
    cabal test
    cabal install

I run this in Bindings/Libgit2 after each new version of libgit2:

    for i in ../../libgit2/include/git2/*.h; do
        c2hsc --prefix=Bindings.Libgit2 \
              --cppopts='-U__BLOCKS__ -I../../libgit2/include' $i
    done
