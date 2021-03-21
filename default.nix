{ compiler    ? "ghc8104"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "4e0d3868c679da20108db402785f924daa1a7fb5"
, sha256      ? "17ypsp6fmyywck1ad6fn77msg2xjlkgwv9ipzb4nw9hpca40hlss"
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
