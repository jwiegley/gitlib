{ mkDerivation, base, base16-bytestring, bytestring, conduit
, conduit-combinators, containers, directory, exceptions, filepath
, hashable, mtl, resourcet, semigroups, stdenv, tagged, text, time
, transformers, unix, unliftio, unliftio-core, unordered-containers
, ...
}:
mkDerivation {
  pname = "gitlib";
  version = "3.1.2";
  src = ./.;
  libraryHaskellDepends = [
    base base16-bytestring bytestring conduit conduit-combinators
    containers directory exceptions filepath hashable mtl resourcet
    semigroups tagged text time transformers unix unliftio
    unliftio-core unordered-containers
  ];
  homepage = "https://github.com/jwiegley/gitlib";
  description = "API library for working with Git repositories";
  license = stdenv.lib.licenses.mit;
}
