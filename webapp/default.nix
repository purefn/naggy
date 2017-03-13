{ mkDerivation, aeson, base, blaze-builder, bytestring, containers
, cryptonite, envparse, exceptions, file-embed, hipbot, http-media
, http-types, lens, lucid, mtl, network, postgresql-simple
, protolude, resource-pool, servant, servant-lucid, servant-server
, stdenv, stm, text, time, tz, unordered-containers, uri-bytestring
, utf8-string, vector, wai-extra, warp
}:
mkDerivation {
  pname = "naggy";
  version = "0.2";
  src = builtins.filterSource (path: type: baseNameOf path != "dist") ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring containers cryptonite envparse
    exceptions file-embed hipbot http-media http-types lens lucid mtl
    network postgresql-simple protolude resource-pool servant
    servant-lucid servant-server stm text time tz unordered-containers
    uri-bytestring utf8-string vector
  ];
  executableHaskellDepends = [ base lens protolude wai-extra warp ];
  homepage = "https://github.com/purefn/naggy";
  description = "A HipChat Reminder Bot";
  license = stdenv.lib.licenses.bsd3;
}
