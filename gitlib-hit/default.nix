{ cabal, conduit, conduitCombinators, gitlib, gitlibTest, hit
, hspec, HUnit, systemFilepath, tagged, text, time, transformers
, unorderedContainers
}:

cabal.mkDerivation (self: {
  pname = "gitlib-hit";
  version = "3.1.0";
  src = ./.;
  buildDepends = [
    conduit conduitCombinators gitlib hit systemFilepath tagged text
    time transformers unorderedContainers
  ];
  testDepends = [
    gitlib gitlibTest hit hspec HUnit systemFilepath time
  ];
  meta = {
    description = "Gitlib repository backend that uses the pure-Haskell Hit library";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
