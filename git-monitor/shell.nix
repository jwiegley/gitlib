with (import <nixpkgs> {}).pkgs;

let devEnv = haskellPackages.override {
  overrides = self: super: {
    git-monitor = self.callPackage ./. {};
    gitlib = self.callPackage ../gitlib {};
    gitlib-libgit2 = self.callPackage ../gitlib-libgit2 {};
  };
 };
in devEnv.git-monitor.env
