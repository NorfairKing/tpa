{ tpa
, opt-env-conf
}:
{ lib
, pkgs
, config
, ...
}:

with lib;

let
  cfg = config.programs.tpa;

in
{
  options = {
    programs.tpa = {
      enable = mkEnableOption "Third party authenticator";
      paths = mkOption ({
        type = types.listOf types.path;
        default = [ ];
        description = "Paths to key files";
      });
    };
  };
  config =
    let
      tpaConfig = {
        "key-paths" = map builtins.toString cfg.paths;
      };
      configFile = (pkgs.formats.yaml { }).generate "tpa-config.yaml" tpaConfig;
      settingsCheck = opt-env-conf.mkSettingsCheck
        "tpa-settings-check"
        "${tpa}/bin/tpa"
        [ "--config-file" configFile ]
        { };
    in
    mkIf cfg.enable {
      xdg.configFile."tpa/config.yaml".source = "${configFile}";
      xdg.configFile."tpa/settings-check.txt".source = "${settingsCheck}";
      home.packages = [ tpa ];
    };
}
