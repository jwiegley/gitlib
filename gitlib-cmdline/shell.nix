with (import <nixpkgs> {}).pkgs;

let devEnv = haskellPackages.override {
  overrides = self: super: {
    gitlib = self.callPackage ../gitlib {};
    gitlib-cmdline = self.callPackage ./. {};
    gitlib-test = self.callPackage ../gitlib-test {};
  };
 };
in devEnv.gitlib-cmdline.env
