{ mkDerivation, base, bytestring, containers, directory, filepath
, gitlib, gitlib-cmdline, hslogger, old-locale
, optparse-applicative, shelly, stdenv, tagged, text, time
, transformers
}:
mkDerivation {
  pname = "git-gpush";
  version = "3.1.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base bytestring containers directory filepath gitlib gitlib-cmdline
    hslogger old-locale optparse-applicative shelly tagged text time
    transformers
  ];
  homepage = "http://github.com/jwiegley/gitlib";
  description = "More intelligent push-to-GitHub utility";
  license = stdenv.lib.licenses.bsd3;
}
