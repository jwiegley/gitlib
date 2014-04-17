{ cabal, filepath, gitlib, gitlibLibgit2, liftedAsync, logging
, monadLogger, optparseApplicative, shelly, tagged, text, time
, transformers, unorderedContainers
}:

cabal.mkDerivation (self: {
  pname = "git-monitor";
  version = "3.1.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    filepath gitlib gitlibLibgit2 liftedAsync logging monadLogger
    optparseApplicative shelly tagged text time transformers
    unorderedContainers
  ];
  meta = {
    homepage = "http://github.com/jwiegley/gitlib";
    description = "Passively snapshots working tree changes efficiently";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
