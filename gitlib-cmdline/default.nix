{ mkDerivation, base, bytestring, conduit, conduit-combinators
, containers, directory, exceptions, gitlib, gitlib-test, hspec
, hspec-expectations, monad-control, mtl, old-locale, parsec
, process-extras, shelly, stdenv, system-filepath, tagged, text
, time, time-locale-compat, transformers, transformers-base
, unordered-containers
}:
mkDerivation {
  pname = "gitlib-cmdline";
  version = "3.1.0.1";
  src = ./.;
  buildDepends = [
    base bytestring conduit conduit-combinators containers directory
    exceptions gitlib monad-control mtl old-locale parsec
    process-extras shelly system-filepath tagged text time
    time-locale-compat transformers transformers-base
    unordered-containers
  ];
  testDepends = [
    base gitlib gitlib-test hspec hspec-expectations system-filepath
    tagged text transformers
  ];
  description = "Gitlib repository backend that uses the git command-line tool";
  license = stdenv.lib.licenses.mit;
}
