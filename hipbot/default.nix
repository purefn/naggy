{ mkDerivation, aeson, base, bifunctors, blaze-builder, bytestring
, cryptonite, either, exceptions, file-embed, http-client
, http-client-tls, http-types, jose-jwt, lens, monad-logger, mtl
, postgresql-simple, postgresql-simple-migration, resource-pool
, safe, servant, servant-server, stdenv, stm, text, time
, transformers, unordered-containers, uri-bytestring, utf8-string
, wai, wreq
}:
mkDerivation {
  pname = "hipbot";
  version = "0.7";
  src = builtins.filterSource (path: type: baseNameOf path != "dist") ./.;
  libraryHaskellDepends = [
    aeson base bifunctors blaze-builder bytestring cryptonite either
    exceptions file-embed http-client http-client-tls http-types
    jose-jwt lens monad-logger mtl postgresql-simple
    postgresql-simple-migration resource-pool safe servant
    servant-server stm text time transformers unordered-containers
    uri-bytestring utf8-string wai wreq
  ];
  homepage = "https://github.com/purefn/hipbot";
  description = "A library for building HipChat Bots";
  license = stdenv.lib.licenses.bsd3;
}
