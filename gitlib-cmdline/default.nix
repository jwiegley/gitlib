{ cabal, conduit, conduitCombinators, exceptions, gitlib
, gitlibTest, hspec, hspecExpectations, monadControl, mtl, parsec
, processExtras, shelly, systemFilepath, tagged, text, time
, transformers, transformersBase, unorderedContainers, git
}:

cabal.mkDerivation (self: {
  pname = "gitlib-cmdline";
  version = "3.1.0";
  src = ./.;
  buildDepends = [
    conduit conduitCombinators exceptions gitlib monadControl mtl
    parsec processExtras shelly systemFilepath tagged text time
    transformers transformersBase unorderedContainers
  ];
  testDepends = [
    gitlib gitlibTest hspec hspecExpectations systemFilepath tagged
    text transformers git
  ];
  meta = {
    description = "Gitlib repository backend that uses the git command-line tool";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
