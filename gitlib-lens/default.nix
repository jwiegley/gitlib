{ mkDerivation, base, bytestring, containers, gitlib
, gitlib-libgit2, hspec, hspec-expectations, HUnit, lens, stdenv
, tagged, template-haskell, text, time, transformers
, unordered-containers
}:
mkDerivation {
  pname = "gitlib-lens";
  version = "3.1.0";
  src = ./.;
  buildDepends = [
    base bytestring containers gitlib lens tagged template-haskell text
    unordered-containers
  ];
  testDepends = [
    base gitlib gitlib-libgit2 hspec hspec-expectations HUnit lens time
    transformers
  ];
  description = "Lenses for working with gitlib more conveniently";
  license = stdenv.lib.licenses.mit;
}
