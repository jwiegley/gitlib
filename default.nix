{ pkgs ? (import <nixpkgs> { config = {
    allowUnfree = true;         # because we haven't set license params
    allowBroken = true;
  };})
}:

let
  haskellPkgs = pkgs.haskell-ng.packages.ghc784;

  inherit (pkgs) stdenv;
  inherit (pkgs.haskell-ng.lib) dontCheck dontHaddock;

  callPackage = stdenv.lib.callPackageWith
    (pkgs // haskellPkgs // haskellDeps // gitlib);

  gitlib = {
    gitlib         = callPackage ./gitlib         {};
    gitlib-cmdline = callPackage ./gitlib-cmdline {};
    gitlib-cross   = callPackage ./gitlib-cross   {};
    gitlib-github  = callPackage ./gitlib-github  {};
    gitlib-hit     = callPackage ./gitlib-hit     {};
    gitlib-lens    = callPackage ./gitlib-lens    {};
    hlibgit2       = callPackage ./hlibgit2       {};
    gitlib-libgit2 = callPackage ./gitlib-libgit2 {};
    gitlib-s3      = callPackage ./gitlib-s3      {};
    gitlib-sample  = callPackage ./gitlib-sample  {};
    gitlib-test    = callPackage ./gitlib-test    {};
    gitlib-utils   = callPackage ./gitlib-utils   {};
    git-gpush      = callPackage ./git-gpush      {};
    git-monitor    = callPackage ./git-monitor    {};
  };

  haskellDeps = pkgs.recurseIntoAttrs {
    # #filepath = haskellPkgs.filepath_1_4_0_0;
    # filepath = callPackage ./nix/filepath.nix {};
    transformers  =
      if pkgs.stdenv.lib.versionOlder haskellPkgs.ghc.version "7.7"
      then haskellPkgs.transformers_0_3_0_0
      else haskellPkgs.transformers;
  };

in {
  gitlib = gitlib;
  deps   = haskellDeps;

  gitlib-libgit2 = gitlib.gitlib-libgit2;

  gitlibTestEnv = with haskellPkgs; with gitlib; pkgs.myEnvFun {
    name = "gitlibTest";
    buildInputs = [ gitlib-libgit2 gitlib-cmdline ] ++ [
      ghc cabal-install
    ];
  };
}
