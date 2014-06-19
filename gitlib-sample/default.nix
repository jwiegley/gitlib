{ cabal, exceptions, gitlib, mtl, transformers }:

cabal.mkDerivation (self: {
  pname = "gitlib-sample";
  version = "3.1.0";
  src = ./.;
  buildDepends = [ exceptions gitlib mtl transformers ];
  meta = {
    description = "Sample backend for gitlib showing the basic structure for any backend";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
