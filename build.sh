set -e
git submodule init
git submodule update
pushd .
cd libgit2
rm -rf build
mkdir build
cd build
cmake .. 
cmake --build .
popd

echo "Starting to build haskell bindings"
cabal configure --user --enable-test
cabal build
cabal test
cabal install --user
