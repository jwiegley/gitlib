{ compiler    ? "ghc822"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "49bdae006e66e70ad3245a463edc01b5749250d3"
, sha256      ? "1ijsifmap47nfzg0spny94lmj66y3x3x8i6vs471bnjamka3dx8p"
, pkgs        ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let haskellPackages = pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  root = ./.;

  source-overrides = {
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    inherit doBenchmark;
  });

  inherit returnShellEnv;
}
