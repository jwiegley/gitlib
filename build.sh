set -e
git submodule init
git submodule update
pushd .
cd libgit2
rm -rf build
mkdir build
cd build
cmake .. -DTHREADSAFE=ON
cmake --build . --target install
popd
