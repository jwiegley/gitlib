let
  pkgs = import <nixpkgs> {};
  hlibgit2-src = pkgs.stdenv.mkDerivation {
    name = "hlibgit2-src";
    src = ./.;
    buildInputs = with pkgs; [
      cmake
      pkg-config
      python3
      zlib
      libssh2
      openssl
      pcre
      http-parser
      libiconv
    ];
    phases = [
      "unpackPhase"
      "configurePhase"
      "installPhase"
    ];
    configurePhase = ''
      cd libgit2
      cmake . \
        -D USE_SHA256=HTTPS \
        -D USE_HTTP_PARSER=system \
        -D REGEX_BACKEND=pcre \
        -D USE_NTLMCLIENT=OFF
      cd ..
    '';
    installPhase = ''
      mkdir $out
      cp -r . $out
    '';
  };
in
  pkgs.haskellPackages.callCabal2nix "hlibgit2" ./. {inherit (pkgs) git;}
