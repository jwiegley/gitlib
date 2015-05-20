{ mkDerivation, aeson, attempt, base, base64-bytestring, binary
, bytestring, conduit, containers, exceptions, filepath, github
, gitlib, hex, http-conduit, monad-control, network, old-locale
, rest-client, shakespeare-text, stdenv, stringable, tagged, text
, time, transformers, transformers-base, unordered-containers, yaml
}:
mkDerivation {
  pname = "gitlib-github";
  version = "1.1.1";
  src = ./.;
  buildDepends = [
    aeson attempt base base64-bytestring binary bytestring conduit
    containers exceptions filepath github gitlib hex http-conduit
    monad-control network old-locale rest-client shakespeare-text
    stringable tagged text time transformers transformers-base
    unordered-containers yaml
  ];
  description = "Gitlib repository backend for storing Git objects on GitHub";
  license = stdenv.lib.licenses.mit;
}
