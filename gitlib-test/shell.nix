with (import <nixpkgs> {}).pkgs;

let devEnv = haskellPackages.override {
  overrides = self: super: {
    gitlib = self.callPackage ../gitlib {};
    gitlib-test = self.callPackage ./. {};
  };
 };
in devEnv.gitlib-test.env
