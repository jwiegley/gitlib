with (import <nixpkgs> {}).pkgs;

let devEnv = haskellPackages.override {
  overrides = self: super: {
    gitlib = self.callPackage ../gitlib {};
    gitlib-libgit2 = self.callPackage ./. {};
    gitlib-test = self.callPackage ../gitlib-test {};
    hgitlib2 = self.callPackage ../hgitlib2 {};
  };
 };
in devEnv.gitlib-libgit2.env
