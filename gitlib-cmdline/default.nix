{ mkDerivation, base, bytestring, conduit, conduit-combinators
, containers, directory, exceptions, git, gitlib, gitlib-test
, hspec, hspec-expectations, monad-control, mtl, old-locale, parsec
, process-extras, shelly, stdenv, system-filepath, tagged, text
, time, time-locale-compat, transformers, transformers-base
, unordered-containers
, ...
}:
mkDerivation {
  pname = "gitlib-cmdline";
  version = "3.1.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit conduit-combinators containers directory
    exceptions gitlib monad-control mtl old-locale parsec
    process-extras shelly system-filepath tagged text time
    time-locale-compat transformers transformers-base
    unordered-containers
  ];
  testHaskellDepends = [
    base gitlib gitlib-test hspec hspec-expectations system-filepath
    tagged text transformers
  ];
  testToolDepends = [ git ];
  description = "Gitlib repository backend that uses the git command-line tool";
  license = stdenv.lib.licenses.mit;
}
