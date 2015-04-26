{ mkDerivation, base, base16-bytestring, bytestring, conduit
, conduit-combinators, containers, directory, exceptions, filepath
, hashable, lifted-async, lifted-base, monad-control, monad-logger
, mtl, resourcet, semigroups, stdenv, system-filepath, tagged, text
, time, transformers, unix, unordered-containers
}:
mkDerivation {
  pname = "gitlib";
  version = "3.1.0.1";
  src = ./.;
  buildDepends = [
    base base16-bytestring bytestring conduit conduit-combinators
    containers directory exceptions filepath hashable lifted-async
    lifted-base monad-control monad-logger mtl resourcet semigroups
    system-filepath tagged text time transformers unix
    unordered-containers
  ];
  description = "API library for working with Git repositories";
  license = stdenv.lib.licenses.mit;
}
