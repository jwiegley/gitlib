{
  description = "Library for manipulating Git repositories";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.gitlib-repo.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          gitlib-repo =
            final.haskell-nix.project' {
              src = ./.;
              supportHpack = true;
              compiler-nix-name = "ghc910";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                # hlint = {};
              };
              shell.buildInputs = with pkgs; [
                pkg-config
              ];
            };
        })
      ];
    in {
      packages.default = flake.packages."git-monitor:exe:git-monitor";
      devShell = flake.devShell // {
        withHoogle = true;
      };
    });
}
