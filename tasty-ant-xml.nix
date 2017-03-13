{ mkDerivation, base, containers, directory, filepath
, generic-deriving, ghc-prim, mtl, stdenv, stm, tagged, tasty
, transformers, xml
}:
mkDerivation {
  pname = "tasty-ant-xml";
  version = "1.0.5";
  sha256 = "0djlj91bnhqq83hbm57ljwixf5zhqk94kb1kgmdh5i74rh7l8bb4";
  libraryHaskellDepends = [
    base containers directory filepath generic-deriving ghc-prim mtl
    stm tagged tasty transformers xml
  ];
  homepage = "http://github.com/ocharles/tasty-ant-xml";
  description = "Render tasty output to XML for Jenkins";
  license = stdenv.lib.licenses.bsd3;
}
