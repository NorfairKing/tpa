{
  description = "tpa";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-24.05";
    home-manager.url = "github:nix-community/home-manager?ref=release-24.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , pre-commit-hooks
    , weeder-nix
    , safe-coloured-text
    , autodocodec
    , opt-env-conf
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (weeder-nix + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (autodocodec + "/nix/overlay.nix"))
          (import (opt-env-conf + "/nix/overlay.nix"))
        ];
      };
      pkgsMusl = pkgs.pkgsMusl;

    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        default = self.packages.${system}.dynamic;
        dynamic = pkgs.tpa;
        static = pkgsMusl.tpa;
      };
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
        buildInputs = with pkgs; [
          zlib
          cabal-install
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      homeManagerModules.${system} = {
        default = self.homeManagerModules.${system}.dynamic;
        static = import ./nix/home-manager-module.nix {
          tpa = self.packages.${system}.static;
          opt-env-conf = pkgsMusl.haskellPackages.opt-env-conf;
        };
        dynamic = import ./nix/home-manager-module.nix {
          tpa = self.packages.${system}.static;
          opt-env-conf = pkgsMusl.haskellPackages.opt-env-conf;
        };
      };
    };
}
