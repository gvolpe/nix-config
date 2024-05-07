{ lib, ... }:

with lib;

{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.browser = {
    settings.dpi = mkOption {
      type = types.str;
      default = "0";
    };
  };
}
