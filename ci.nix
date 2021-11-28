let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit-hooks = import ./nix/pre-commit.nix { inherit sources; };
in
pkgs.totpPackages // {
  pre-commit-check = pre-commit-hooks.run;
}
