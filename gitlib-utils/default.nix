{ mkDerivation, base, bytestring, conduit, exceptions, filepath
, gitlib, hex, lifted-base, stdenv, system-fileio, tagged, text
, transformers, unordered-containers
}:
mkDerivation {
  pname = "gitlib-utils";
  version = "1.3.2";
  src = ./.;
  buildDepends = [
    base bytestring conduit exceptions filepath gitlib hex lifted-base
    system-fileio tagged text transformers unordered-containers
  ];
  description = "This package is now deprecated";
  license = stdenv.lib.licenses.mit;
}
