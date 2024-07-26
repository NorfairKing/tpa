{ lib }:
{
  key-paths = lib.mkOption {
    default = null;
    description = "key paths";
    type = lib.types.nullOr (lib.types.listOf lib.types.str);
  };
}
