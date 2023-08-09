{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlay = pkgsNew: pkgsOld: {
          ninem = pkgsNew.haskell.lib.justStaticExecutables
            pkgsNew.haskellPackages.ninem;

          haskellPackages = pkgsOld.haskellPackages.override (old: {
            overrides =
              pkgsNew.haskell.lib.packageSourceOverrides { ninem = ./.; };
          });
        };

        pkgs = import nixpkgs {
          inherit config system;
          overlays = [ overlay ];
        };

      in rec {
        packages.default = pkgs.haskellPackages.ninem;

        apps.default = {
          type = "app";
          program = "${pkgs.ninem}/bin/9m";
        };

        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              haskellPackages.haskell-language-server
              haskellPackages.hlint
              haskellPackages.cabal-fmt
              haskellPackages.ormolu
              cabal-install
              zlib
            ];
          };
        };
      });
}
