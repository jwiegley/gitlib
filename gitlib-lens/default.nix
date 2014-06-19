{ cabal, gitlib, gitlibLibgit2, hspec, hspecExpectations, HUnit
, lens, tagged, text, time, transformers, unorderedContainers
}:

cabal.mkDerivation (self: {
  pname = "gitlib-lens";
  version = "3.1.0";
  src = ./.;
  buildDepends = [ gitlib lens tagged text unorderedContainers ];
  testDepends = [
    gitlib gitlibLibgit2 hspec hspecExpectations HUnit lens time
    transformers
  ];
  meta = {
    description = "Lenses for working with gitlib more conveniently";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
