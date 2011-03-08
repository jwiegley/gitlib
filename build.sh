set -e
git submodule init
git submodule update

echo 
echo "Generating bindings"
ruby ./create_bindings.rb

echo 
echo "Starting to build haskell bindings"
cabal configure --user --enable-test
cabal build
cabal test
cabal install --user
