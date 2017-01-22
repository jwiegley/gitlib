{ mkDerivation, base, base16-bytestring, bytestring, containers
, directory, exceptions, filepath, hashable, lifted-async
, lifted-base, monad-control, mtl, semigroups, stdenv, streaming
, tagged, template-haskell, text, time, transformers
, transformers-base, unix, unordered-containers, mmorph
, cabal-install, resourcet, streaming-bytestring
}:
mkDerivation {
  pname = "gitlib";
  version = "4.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base16-bytestring bytestring containers directory exceptions
    filepath hashable lifted-async lifted-base monad-control mtl
    semigroups streaming tagged template-haskell text time transformers
    transformers-base unix unordered-containers cabal-install mmorph
    resourcet streaming-bytestring
  ];
  description = "API library for working with Git repositories";
  license = stdenv.lib.licenses.mit;
}
