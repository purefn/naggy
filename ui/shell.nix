let
  config = import ../config.nix;
  pkgs = import <nixpkgs> { inherit config; };
  pkg = pkgs.haskell.packages.ghcjs.naggy-ui;
  buildInputs = pkg.buildInputs ++ pkg.nativeBuildInputs ++ pkg.propagatedNativeBuildInputs ++ pkg.propagatedBuildInputs;
  ghc = pkgs.haskell.packages.ghcjs.ghcWithPackages(ps: buildInputs);
in
  pkgs.stdenv.mkDerivation {
    name = "naggy-haskell-env";
    buildInputs = [ ghc pkgs.cabal-install ];
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
  }

