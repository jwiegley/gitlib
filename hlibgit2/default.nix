{ mkDerivation, base, bindings-DSL, git, openssl, process, stdenv
, zlib
, ...
}:
mkDerivation {
  pname = "hlibgit2";
  version = "0.18.0.16";
  src = ./.;
  libraryHaskellDepends = [ base bindings-DSL zlib ];
  librarySystemDepends = [ openssl ];
  testHaskellDepends = [ base process ];
  testToolDepends = [ git ];
  description = "Low-level bindings to libgit2";
  license = stdenv.lib.licenses.mit;
}
