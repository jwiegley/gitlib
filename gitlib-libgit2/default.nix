let
  pkgs = import <nixpkgs> {};
  hpkgs = pkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      gitlib = hsuper.callCabal2nix "gitlib" ../gitlib {};
      gitlib-test = hsuper.callCabal2nix "gitlib-test" ../gitlib-test {};
      hlibgit2 = import ../hlibgit2;
    };
  };
in
  hpkgs.callCabal2nix "gitlib-libgit2" ./. {}
