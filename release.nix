let
  config = import ./config.nix;
  pkgs = import <nixpkgs> { inherit config; };
in {
  hipbot = pkgs.haskellPackages.hipbot;

  naggy-webapp = pkgs.haskellPackages.naggy-webapp;

  naggy-ui = pkgs.haskell.packages.ghcjs.naggy-ui;

  naggy-webapp-minimal = pkgs.naggy-webapp-minimal;

  docker-container = pkgs.docker-container;
}
