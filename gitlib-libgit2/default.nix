{ mkDerivation, base, bytestring, conduit, conduit-combinators
, containers, directory, exceptions, fast-logger, filepath, gitlib
, gitlib-test, hlibgit2, hspec, hspec-expectations, HUnit
, lifted-async, lifted-base, mmorph, monad-control, monad-logger
, monad-loops, mtl, resourcet, stdenv, stm, stm-conduit, tagged
, template-haskell, text, text-icu, time, transformers
, transformers-base
}:
mkDerivation {
  pname = "gitlib-libgit2";
  version = "3.1.0.5";
  src = ./.;
  buildDepends = [
    base bytestring conduit conduit-combinators containers directory
    exceptions fast-logger filepath gitlib hlibgit2 lifted-async
    lifted-base mmorph monad-control monad-logger monad-loops mtl
    resourcet stm stm-conduit tagged template-haskell text text-icu
    time transformers transformers-base
  ];
  testDepends = [
    base exceptions gitlib gitlib-test hspec hspec-expectations HUnit
    monad-logger transformers
  ];
  description = "Libgit2 backend for gitlib";
  license = stdenv.lib.licenses.mit;
}
