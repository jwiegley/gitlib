{ mkDerivation, base, exceptions, gitlib, mtl, stdenv, transformers
}:
mkDerivation {
  pname = "gitlib-sample";
  version = "3.1.0";
  src = ./.;
  buildDepends = [ base exceptions gitlib mtl transformers ];
  description = "Sample backend for gitlib showing the basic structure for any backend";
  license = stdenv.lib.licenses.mit;
}
