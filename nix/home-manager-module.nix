{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.tpa;

  toYamlFile = pkgs.callPackage ./to-yaml.nix {};

  tpaCli = (import ./pkgs.nix {}).tpa;
in
{
  options =
    {
      programs.tpa =
        {
          enable = mkEnableOption "Third party authenticator";
          paths = mkOption (
            {
              type = types.listOf types.path;
              default = [];
              description = "Paths to key files";
            }
          );
        };
    };
  config =
    let
      tpaConfig = {
        "key-paths" = cfg.paths;
      };
      configFile = toYamlFile "tpa-config" tpaConfig;
    in
      mkIf cfg.enable {
        xdg.configFile."tpa/config.yaml".source = "${configFile}";
        home.packages = [ tpaCli ];
      };
}
