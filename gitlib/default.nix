{ mkDerivation, base, base16-bytestring, bytestring, pipes
, pipes-group, containers, directory, exceptions, filepath
, hashable, lifted-async, lifted-base, monad-control
, mtl, semigroups, stdenv, tagged, text, time, free
, transformers, unix, unordered-containers, pipes-safe
, transformers-base, template-haskell
}:
mkDerivation {
  pname = "gitlib";
  version = "4.0.0.0";
  src = ./.;
  buildDepends = [
    base base16-bytestring bytestring pipes pipes-group
    containers directory exceptions filepath hashable lifted-async
    lifted-base monad-control mtl semigroups free pipes-safe
    tagged text time transformers unix unordered-containers
    transformers-base template-haskell
  ];
  description = "API library for working with Git repositories";
  license = stdenv.lib.licenses.mit;
}
