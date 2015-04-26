{ mkDerivation, aeson, attempt, aws, base, bifunctors, binary
, bytestring, conduit, conduit-combinators, data-default, directory
, exceptions, filepath, ghc-prim, gitlib, gitlib-libgit2
, gitlib-test, hlibgit2, hspec, hspec-expectations, http-conduit
, HUnit, lens, lifted-base, monad-control, monad-logger, resourcet
, retry, split, stdenv, stm, template-haskell, temporary, text
, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "gitlib-s3";
  version = "3.1.0.1";
  src = ./.;
  buildDepends = [
    aeson attempt aws base bifunctors binary bytestring conduit
    conduit-combinators data-default directory exceptions filepath
    ghc-prim gitlib gitlib-libgit2 hlibgit2 http-conduit lens
    lifted-base monad-control monad-logger resourcet retry split stm
    template-haskell text time transformers unordered-containers
  ];
  testDepends = [
    aws base data-default directory exceptions filepath gitlib
    gitlib-libgit2 gitlib-test hlibgit2 hspec hspec-expectations HUnit
    monad-logger resourcet temporary text transformers
  ];
  description = "Gitlib repository backend for storing Git objects in Amazon S3";
  license = stdenv.lib.licenses.mit;
}
