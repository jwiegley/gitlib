{ cabal, gitlib, gitlibCmdline, gitlibLibgit2, gitlibTest, hspec
, hspecExpectations, HUnit
}:

cabal.mkDerivation (self: {
  pname = "gitlib-cross";
  version = "3.1.0";
  src = ./.;
  testDepends = [
    gitlib gitlibCmdline gitlibLibgit2 gitlibTest hspec
    hspecExpectations HUnit
  ];
  meta = {
    description = "Run tests between repositories";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
