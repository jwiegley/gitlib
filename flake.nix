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
                    icu
                  ];
                  withHoogle = true;
                  shellHook = let
                    isDarwin = final.stdenv.isDarwin;
                    libExt = if isDarwin then "dylib" else "so";
                  in ''
                    # Create symlink to libgit2 source for cabal builds
                    if [ ! -e hlibgit2/libgit2 ]; then
                      echo "Setting up libgit2 source symlink for hlibgit2..."
                      ln -sf ${libgit2-src} hlibgit2/libgit2
                    fi

                    # Force cabal to use Nix's ICU library instead of system ICU
                    export PKG_CONFIG_PATH="${final.icu}/lib/pkgconfig:$PKG_CONFIG_PATH"
                    export LIBRARY_PATH="${final.icu}/lib:$LIBRARY_PATH"
                    export C_INCLUDE_PATH="${final.icu.dev}/include:$C_INCLUDE_PATH"
                    export CPLUS_INCLUDE_PATH="${final.icu.dev}/include:$CPLUS_INCLUDE_PATH"

                    # Platform-specific runtime library overrides
                    # This is needed because the cached text-icu may have system paths baked in
                  '' + (if isDarwin then ''
                    export DYLD_LIBRARY_PATH="${final.icu}/lib:$DYLD_LIBRARY_PATH"
                    export DYLD_INSERT_LIBRARIES="${final.icu}/lib/libicuuc.${libExt}:${final.icu}/lib/libicui18n.${libExt}:${final.icu}/lib/libicudata.${libExt}"
                  '' else ''
                    export LD_LIBRARY_PATH="${final.icu}/lib:$LD_LIBRARY_PATH"
                    export LD_PRELOAD="${final.icu}/lib/libicuuc.${libExt}:${final.icu}/lib/libicui18n.${libExt}:${final.icu}/lib/libicudata.${libExt}"
                  '') + ''

                    # Create cabal.project.local to explicitly use Nix's ICU for rebuilds
                    cat > cabal.project.local <<EOF
                    package text-icu
                      extra-lib-dirs: ${final.icu}/lib
                      extra-include-dirs: ${final.icu.dev}/include
                    EOF

                    echo "ICU library configured: ${final.icu}/lib"
                  '';
                };
                modules = [{
                  # Force hlibgit2 to use our libgit2 source
                  packages.hlibgit2.src = final.lib.mkForce (final.runCommand "hlibgit2-src" {} ''
                    cp -r ${./.}/hlibgit2 $out
                    chmod -R u+w $out
                    rm -rf $out/libgit2
                    cp -r ${libgit2-src} $out/libgit2
                  '');

                  # Configure text-icu to use Nix's ICU (for nix build)
                  packages.text-icu.components.library = {
                    libs = final.lib.mkForce [ final.icu ];
                    pkgconfig = final.lib.mkForce [ [ final.icu ] ];
                  };
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
