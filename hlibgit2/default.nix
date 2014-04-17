{ cabal, bindingsDSL, zlib }:

cabal.mkDerivation (self: {
  pname = "hlibgit2";
  version = "0.18.0.13";
  src = ./.;
  buildDepends = [ bindingsDSL zlib ];
  extraLibraries = [ ];
  # The test in hlibgit2 is extremely minimal, and rather needlessly requires
  # git to run.  There is little point in requiring it here.
  doCheck = false;
  meta = {
    description = "Low-level bindings to libgit2";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
