{ cabal, conduit, failure, fastLogger, filepath, gitlib, gitlibTest
, hlibgit2, hspec, hspecExpectations, HUnit, liftedAsync
, liftedBase, missingForeign, mmorph, monadControl, monadLogger
, monadLoops, mtl, resourcet, stm, stmConduit, tagged, text
, textIcu, time, transformers, transformersBase
}:

cabal.mkDerivation (self: {
  pname = "gitlib-libgit2";
  version = "3.0.1";
  src = ./.;
  buildDepends = [
    conduit failure fastLogger filepath gitlib hlibgit2 liftedAsync
    liftedBase missingForeign mmorph monadControl monadLogger
    monadLoops mtl resourcet stm stmConduit tagged text textIcu time
    transformers transformersBase
  ];
  testDepends = [ gitlib gitlibTest hspec hspecExpectations HUnit ];
  meta = {
    description = "Libgit2 backend for gitlib";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
