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
  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };

in
{
  options = {
    programs.tpa = {
      enable = mkEnableOption "Third party authenticator";
      config = mkOption {
        default = { };
        type = types.submodule {
          options = (pkgs.callPackage ../tpa/options.nix { });
        };
      };
      extraConfig = mkOption {
        description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
        default = { };
      };
    };
  };
  config =
    let
      tpaConfig = mergeListRecursively [
        (builtins.removeAttrs cfg.config [ "override" "overrideDerivation" ])
        cfg.extraConfig
      ];
      configFile = (pkgs.formats.yaml { }).generate "tpa-config.yaml" tpaConfig;
      settingsCheck = opt-env-conf.makeSettingsCheck
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
