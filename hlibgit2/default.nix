{ mkDerivation, base, bindings-DSL, git, openssl, process, stdenv
, zlib
}:
mkDerivation {
  pname = "hlibgit2";
  version = "0.18.0.16";
  src = ./.;
  buildDepends = [ base bindings-DSL zlib ];
  testDepends = [ base process ];
  buildTools = [ git ];
  extraLibraries = [ openssl ];
  description = "Low-level bindings to libgit2";
  license = stdenv.lib.licenses.mit;
}
