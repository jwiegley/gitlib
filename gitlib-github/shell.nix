with (import <nixpkgs> {}).pkgs;

let devEnv = haskellPackages.override {
  overrides = self: super: {
    gitlib = self.callPackage ../gitlib {};
    gitlib-github = self.callPackage ./. {};
  };
 };
in devEnv.gitlib-github.env
