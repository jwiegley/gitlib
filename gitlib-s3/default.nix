{ cabal, aeson, attempt, aws, bifunctors, binary, conduit
, conduitCombinators, dataDefault, exceptions, filepath, gitlib
, gitlibLibgit2, gitlibTest, hlibgit2, hspec, hspecExpectations
, httpConduit, HUnit, lens, liftedBase, monadControl, monadLogger
, resourcet, retry, split, stm, temporary, text, time, transformers
, unorderedContainers
}:

cabal.mkDerivation (self: {
  pname = "gitlib-s3";
  version = "3.1.0";
  src = ./.;
  buildDepends = [
    aeson attempt aws bifunctors binary conduit conduitCombinators
    dataDefault exceptions filepath gitlib gitlibLibgit2 hlibgit2
    httpConduit lens liftedBase monadControl monadLogger resourcet
    retry split stm text time transformers unorderedContainers
  ];
  testDepends = [
    aws dataDefault exceptions filepath gitlib gitlibLibgit2 gitlibTest
    hlibgit2 hspec hspecExpectations HUnit monadLogger resourcet
    temporary text transformers
  ];
  meta = {
    description = "Gitlib repository backend for storing Git objects in Amazon S3";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
