with (import <nixpkgs> {}).pkgs;

let devEnv = haskellPackages.override {
  overrides = self: super: {
    gitlib = self.callPackage ../gitlib {};
    gitlib-utils = self.callPackage ./. {};
  };
 };
in devEnv.gitlib-utils.env
