{ mkDerivation, aeson, base, bifunctors, blaze-builder, bytestring
, cereal, clientsession, containers, cookie, exceptions, hipbot
, lens, lucid, mtl, safe, stdenv, stm, text, time, transformers, tz
, unordered-containers, utf8-string, uuid, wai, wai-extra, wai-lens
, warp, webcrank-wai
}:
mkDerivation {
  pname = "naggy";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base bifunctors blaze-builder bytestring cereal clientsession
    containers cookie exceptions hipbot lens lucid mtl safe stm text
    time transformers tz unordered-containers utf8-string uuid wai
    wai-extra wai-lens warp webcrank-wai
  ];
  homepage = "https://bitbucket.org/rwallace/naggy";
  description = "A HipChat Reminder Bot";
  license = stdenv.lib.licenses.bsd3;
}
