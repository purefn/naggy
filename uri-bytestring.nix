{ mkDerivation, attoparsec, base, blaze-builder, bytestring
, containers, generics-sop, HUnit, lens-simple, QuickCheck
, quickcheck-instances, semigroups, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, th-lift-instances
}:
mkDerivation {
  pname = "uri-bytestring";
  version = "0.2.3.1";
  sha256 = "0vdiy6z9r5idml6rjbf6h2y24as97j34spcrrwpvgj3nraw18a0x";
  libraryHaskellDepends = [
    attoparsec base blaze-builder bytestring containers
    template-haskell th-lift-instances
  ];
  testHaskellDepends = [
    attoparsec base blaze-builder bytestring containers generics-sop
    HUnit lens-simple QuickCheck quickcheck-instances semigroups tasty
    tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/Soostone/uri-bytestring";
  description = "Haskell URI parsing as ByteStrings";
  license = stdenv.lib.licenses.bsd3;
}
