{
  # ghcjs is marked as broken
  allowBroken = true;

  packageOverrides = pkgs: rec {

    haskellPackages = pkgs.haskellPackages.override {
      overrides = hpkgs: old: rec {
        lens-properties = pkgs.haskell.lib.doJailbreak old.lens-properties;

        # newer version
        postgresql-simple-migration = hpkgs.callPackage ./postgresql-simple-migration.nix {};

        # newer version
        uri-bytestring = hpkgs.callPackage ./uri-bytestring.nix {};

        hipbot = pkgs.haskell.lib.dontHaddock (hpkgs.callPackage ./hipbot/default.nix {});

        naggy-webapp =
          pkgs.haskell.lib.overrideCabal(hpkgs.callPackage ./webapp/default.nix { })(drv: {
            enableSharedExecutables = false;
            doHaddock = false;
          });
      };
    };

    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghcjs = pkgs.haskell.packages.ghcjsHEAD.override {
          overrides = hpkgs: old: rec {
            /* dependent-sum = hpkgs.callPackage ./dependent-sum.nix {}; */
            /* dependent-sum-template = pkgs.haskell.lib.doJailbreak old.dependent-sum-template; */
            /* reflex = pkgs.haskell.lib.doJailbreak old.reflex; */
            /* reflex-dom = pkgs.haskell.lib.doJailbreak old.reflex-dom; */

            react-flux = hpkgs.callPackage ./react-flux.nix {};
            naggy-ui = hpkgs.callPackage ./ui/default.nix {};
          };
        };
      };
    };

    naggy-webapp-minimal = pkgs.stdenv.mkDerivation {
      name = "naggy-webapp-minimal";
      buildCommand = ''
        mkdir -p $out/bin
        cp ${haskellPackages.naggy-webapp}/bin/naggy $out/bin/naggy
      '';
    };

    docker-container = pkgs.dockerTools.buildImage {
      name = "naggy";
      fromImage = pkgs.dockerTools.buildImage {
        name = "alpine";
        contents = pkgs.cacert;
        fromImage = pkgs.dockerTools.buildImage {
          name = "alpine";
          runAsRoot = ''
            echo "hosts: files dns myhostname mymachines" > /etc/nsswitch.conf
          '';
          fromImage = pkgs.dockerTools.pullImage {
            imageName = "alpine";
            imageTag = "3.3";
            imageId = "f58d61a874bedb7cdcb5a409ebb0c53b0656b880695c14e78a69902873358d5f";
            sha256 = "0lvd5zxjgwp3jl5r8qgb2kapmxclpgdv1a7c03yiagxsil5gwb8c";
          };
        };
      };
      contents = naggy-webapp-minimal;
      config = {
        Cmd = [ "${naggy-webapp-minimal}/bin/naggy" ];
        ExposedPorts = {
          "8080/tcp" = {};
        };
      };
    };
  };
}

