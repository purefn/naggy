let
  pkgs = import <nixpkgs> {};
  pkg = (import ../release.nix).hipbot;
  buildInputs = pkg.buildInputs ++ pkg.nativeBuildInputs ++ pkg.propagatedNativeBuildInputs ++ pkg.propagatedBuildInputs;
  tools = ps: with ps; [ cabal-install ghc-mod ghcid ];
  ghc = pkgs.haskellPackages.ghcWithPackages(ps: buildInputs ++ tools ps);
in
  pkgs.stdenv.mkDerivation {
    name = "naggy-haskell-env";
    buildInputs = [ ghc ];
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
  }

