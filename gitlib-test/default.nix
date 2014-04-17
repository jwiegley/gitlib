{ cabal, conduit, failure, gitlib, hspec, hspecExpectations, HUnit
, monadControl, tagged, text, time, transformers
}:

cabal.mkDerivation (self: {
  pname = "gitlib-test";
  version = "3.0.1";
  src = ./.;
  buildDepends = [
    conduit failure gitlib hspec hspecExpectations HUnit monadControl
    tagged text time transformers
  ];
  meta = {
    description = "Test library for confirming gitlib backend compliance";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
