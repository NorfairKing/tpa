{
  description = "tpa";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.11";
    home-manager.url = "github:nix-community/home-manager?ref=release-23.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , pre-commit-hooks
    , weeder-nix
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (weeder-nix + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;

    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.tpa;
      checks.${system} = {
        nixos-module-test = import ./nix/nixos-module-test.nix {
          inherit pkgs;
          home-manager = home-manager.nixosModules.home-manager;
          tpa-home-manager-module = self.homeManagerModules.${system}.default;
        };
        weeder-check = pkgs.weeder-nix.makeWeederCheck {
          weederToml = ./weeder.toml;
          packages = [ "tpa" ];
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "tpa-shell";
        packages = p: [ p.tpa ];
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          zlib
          cabal-install
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      homeManagerModules.${system}.default = import ./nix/home-manager-module.nix { tpa = pkgs.tpa; };
    };
}
