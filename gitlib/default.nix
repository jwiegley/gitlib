{ cabal, base16Bytestring, conduit, failure, filepath, hashable
, liftedBase, monadControl, monadLogger, mtl, resourcet, semigroups
, tagged, text, time, transformers, unorderedContainers
, liftedAsync, conduitCombinators, systemFilepath
}:

cabal.mkDerivation (self: {
  pname = "gitlib";
  version = "3.0.2";
  src = ./.;
  buildDepends = [
    base16Bytestring conduit failure filepath hashable liftedBase
    monadControl monadLogger mtl resourcet semigroups tagged text time
    transformers unorderedContainers
    liftedAsync conduitCombinators systemFilepath
  ];
  meta = {
    description = "API library for working with Git repositories";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
