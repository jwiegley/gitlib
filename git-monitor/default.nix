{ mkDerivation, base, bytestring, containers, directory, filepath
, gitlib, gitlib-libgit2, lifted-async, logging, old-locale
, optparse-applicative, shelly, stdenv, tagged, template-haskell
, text, time, transformers, unix, unordered-containers
, ...
}:
mkDerivation {
  pname = "git-monitor";
  version = "3.1.1.5";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers directory filepath gitlib gitlib-libgit2
    lifted-async logging old-locale optparse-applicative shelly tagged
    template-haskell text time transformers unix unordered-containers
  ];
  homepage = "http://github.com/jwiegley/gitlib";
  description = "Passively snapshots working tree changes efficiently";
  license = stdenv.lib.licenses.bsd3;
}
