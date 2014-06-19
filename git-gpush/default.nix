{ cabal, filepath, gitlib, gitlibCmdline, hslogger
, optparseApplicative, shelly, tagged, text, time, transformers
}:

cabal.mkDerivation (self: {
  pname = "git-gpush";
  version = "3.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    filepath gitlib gitlibCmdline hslogger optparseApplicative shelly
    tagged text time transformers
  ];
  meta = {
    homepage = "http://github.com/jwiegley/gitlib";
    description = "More intelligent push-to-GitHub utility";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
