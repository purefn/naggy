{ mkDerivation, base, containers, criterion, deepseq, dependent-map
, dependent-sum, exception-transformers, haskell-src-exts
, haskell-src-meta, MemoTrie, mtl, primitive, ref-tf, semigroups
, split, stdenv, stm, syb, template-haskell, these, transformers
, transformers-compat
}:
mkDerivation {
  pname = "reflex";
  version = "0.4.0";
  sha256 = "173b8ysrghrw2fvdsqf6ybik9f24kw4ji1h8w4wj5kspbi12s36n";
  revision = "2";
  editedCabalFile = "e88ff0200373c04d57ff4b3232ada2a6965f61a23d99a8ef6bcbf96603c9d992";
  libraryHaskellDepends = [
    base containers dependent-map dependent-sum exception-transformers
    haskell-src-exts haskell-src-meta mtl primitive ref-tf semigroups
    syb template-haskell these transformers transformers-compat
  ];
  testHaskellDepends = [
    base containers dependent-map MemoTrie mtl ref-tf
  ];
  homepage = "https://github.com/reflex-frp/reflex";
  description = "Higher-order Functional Reactive Programming";
  license = stdenv.lib.licenses.bsd3;
}
