{ mkDerivation, base, bytestring, conduit, conduit-combinators
, containers, directory, gitlib, gitlib-test, hit, hspec, HUnit
, old-locale, stdenv, system-filepath, tagged, text, time
, transformers, unordered-containers
}:
mkDerivation {
  pname = "gitlib-hit";
  version = "3.1.0";
  src = ./.;
  buildDepends = [
    base bytestring conduit conduit-combinators containers directory
    gitlib hit old-locale system-filepath tagged text time transformers
    unordered-containers
  ];
  testDepends = [
    base bytestring gitlib gitlib-test hit hspec HUnit old-locale
    system-filepath time
  ];
  description = "Gitlib repository backend that uses the pure-Haskell Hit library";
  license = stdenv.lib.licenses.mit;
}
