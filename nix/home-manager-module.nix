{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.intray;

  toYamlFile = pkgs.callPackage ./to-yaml.nix {};

  tpaPackages = (import ./pkgs.nix {}).tpaPackages;
in
{
  options =
    {
      programs.tpa =
        {
          enable = mkEnableOption "Third party authenticator";
          paths = mkOption (
            {
              type = types.listOf types.str;
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
        xdg.configFile."tpa/config.yaml".source = "${intrayConfigFile}";
        home.packages = [ tpaPackages.tpa ];
      };
}
