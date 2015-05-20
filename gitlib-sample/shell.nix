with (import <nixpkgs> {}).pkgs;

let devEnv = haskellPackages.override {
  overrides = self: super: {
    gitlib = self.callPackage ../gitlib {};
    gitlib-sample = self.callPackage ./. {};
  };
 };
in devEnv.gitlib-sample.env
