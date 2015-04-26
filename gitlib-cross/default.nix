{ mkDerivation, base, gitlib, gitlib-cmdline, gitlib-libgit2
, gitlib-test, hspec, hspec-expectations, HUnit, stdenv
}:
mkDerivation {
  pname = "gitlib-cross";
  version = "3.1.0";
  src = ./.;
  buildDepends = [ base ];
  testDepends = [
    base gitlib gitlib-cmdline gitlib-libgit2 gitlib-test hspec
    hspec-expectations HUnit
  ];
  description = "Run tests between repositories";
  license = stdenv.lib.licenses.mit;
}
