{ mkDerivation, base, QuickCheck, random, stdenv }:
mkDerivation {
  pname = "filepath";
  version = "1.3.0.2";
  revision = "2";
  sha256 = "0wvvz6cs5fh4f04a87b9s7xrnzypmnzzkn149p6xk8xi7gcvcpy2";
  editedCabalFile = "42c2b0c550f4c73d044f186a58e34285632705b6936dc24fb6012eb10bf70cc7";
  buildDepends = [ base ];
  testDepends = [ base QuickCheck random ];
  homepage = "http://www-users.cs.york.ac.uk/~ndm/filepath/";
  description = "Library for manipulating FilePaths in a cross platform way";
  license = stdenv.lib.licenses.bsd3;
}
