{ mkDerivation, aeson, base, ghcjs-base, lens, mtl, protolude
, react-flux, stdenv, text, time, tz
}:
mkDerivation {
  pname = "naggy-ui";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != "dist") ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base ghcjs-base lens mtl protolude react-flux text time tz
  ];
  license = stdenv.lib.licenses.bsd3;
}
