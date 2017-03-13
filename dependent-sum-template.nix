{ mkDerivation, base, dependent-sum, stdenv, template-haskell
, th-extras
}:
mkDerivation {
  pname = "dependent-sum-template";
  version = "0.0.0.5";
  sha256 = "0r87c0m2cjgvp62r84ki7nkaimdyjsv6l62sc8xvrn55ld6mhgxj";
  libraryHaskellDepends = [
    base dependent-sum template-haskell th-extras
  ];
  homepage = "/dev/null";
  description = "Template Haskell code to generate instances of classes in dependent-sum package";
  license = stdenv.lib.licenses.publicDomain;
}
