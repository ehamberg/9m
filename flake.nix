{
  description = "9m";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskell.packages.ghc92;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "9m";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

        packages.default = self.packages.${system}.${packageName};

        apps.${packageName} = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/9m";
        };

        apps.default = self.apps.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.cabal-fmt
            haskellPackages.ormolu
            cabal-install
            zlib
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
