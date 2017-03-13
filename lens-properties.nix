{ mkDerivation, base, lens, QuickCheck, stdenv, transformers }:
mkDerivation {
  pname = "lens-properties";
  version = "4.11";
  sha256 = "0cg0n75ss5ayy31igwyz9yz2sh0smcaiidbbm1wkrk1krzbws31w";
  libraryHaskellDepends = [ base lens QuickCheck transformers ];
  jailbreak = true;
  homepage = "http://github.com/ekmett/lens/";
  description = "QuickCheck properties for lens";
  license = stdenv.lib.licenses.bsd3;
}
