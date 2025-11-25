{
  description = "Org data tools";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
    libgit2-src = {
      url = "github:jwiegley/libgit2/1dbd0b6fe7752657416fc1c69600fbe9c6e04d61";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, libgit2-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            gitlib =
              final.haskell-nix.project' {
                src = ./.;
                supportHpack = true;
                compiler-nix-name = "ghc910";
                shell = {
                  tools = {
                    cabal = {};
                    haskell-language-server = {};
                  };
                  buildInputs = with final; [
                    pkg-config
                  ];
                  withHoogle = true;
                };
                modules = [{
                  packages.hlibgit2.src = final.lib.mkForce (final.runCommand "hlibgit2-src" {} ''
                    cp -r ${./.}/hlibgit2 $out
                    chmod -R u+w $out
                    rm -rf $out/libgit2
                    cp -r ${libgit2-src} $out/libgit2
                  '');
                }];
              };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.gitlib.flake {
        };
      in flake // {
        packages.default = flake.packages."git-monitor:exe:git-monitor";
        devShells.default = flake.devShells.default // {
          withHoogle = true;
        };
      });
}
