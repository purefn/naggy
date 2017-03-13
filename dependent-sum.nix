{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "dependent-sum";
  version = "0.4";
  sha256 = "07hs9s78wiybwjwkal2yq65hdavq0gg1h2ld7wbph61s2nsfrpm8";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/mokus0/dependent-sum";
  description = "Dependent sum type";
  license = stdenv.lib.licenses.publicDomain;
}
