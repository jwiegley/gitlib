{ mkDerivation, base, bytestring, pipes, pipes-group
, containers, directory, exceptions, fast-logger, filepath, gitlib
, hlibgit2, hspec, hspec-expectations, HUnit
, lifted-async, lifted-base, mmorph, monad-control
, monad-loops, mtl, stdenv, stm, tagged, free
, template-haskell, text, text-icu, time, transformers
, transformers-base
}:
mkDerivation {
  pname = "gitlib-libgit2";
  version = "4.0.0.0";
  src = ./.;
  buildDepends = [
    base bytestring pipes pipes-group containers directory
    exceptions fast-logger filepath gitlib hlibgit2 lifted-async
    lifted-base mmorph monad-control monad-loops mtl
    stm tagged template-haskell text text-icu
    time transformers transformers-base free
  ];
  testDepends = [
    base exceptions gitlib hspec hspec-expectations HUnit
    transformers
  ];
  description = "Libgit2 backend for gitlib";
  license = stdenv.lib.licenses.mit;
}
