{ mkDerivation, aeson, base, bifunctors, bytestring, containers
, data-default, dependent-map, dependent-sum
, dependent-sum-template, directory, exception-transformers
, ghcjs-dom, glib, gtk3, lens, mtl, raw-strings-qq, ref-tf, reflex
, safe, semigroups, stdenv, text, these, time, transformers, unix
, webkitgtk3, webkitgtk3-javascriptcore
}:
mkDerivation {
  pname = "reflex-dom";
  version = "0.3";
  sha256 = "0fldnl2yamn24v0qnfr4hhy4q9nq6kxspiy39yk5kdfvxg8aqax5";
  revision = "2";
  editedCabalFile = "b569e2b08dac72a37173f680be5eaeb9ad57900c08301bf7b958f1cf52ac6055";
  libraryHaskellDepends = [
    aeson base bifunctors bytestring containers data-default
    dependent-map dependent-sum dependent-sum-template directory
    exception-transformers ghcjs-dom glib gtk3 lens mtl raw-strings-qq
    ref-tf reflex safe semigroups text these time transformers unix
    webkitgtk3 webkitgtk3-javascriptcore
  ];
  description = "Functional Reactive Web Apps with Reflex";
  license = stdenv.lib.licenses.bsd3;
}
