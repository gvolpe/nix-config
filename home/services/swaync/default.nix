{ config, lib, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;

  cfg = config.services.swaync;
  filePath = "${config.dotfiles.path}/services/swaync";
  jsonConfig = builtins.fromJSON (builtins.readFile ./config.json);

  style = config.dotfiles.make ./style.css "services/swaync";
in
{
  services.swaync = {
    inherit style;
    enable = true;
    settings = lib.mkIf (!config.dotfiles.mutable) (
      jsonConfig // {
        "$schema" = "${cfg.package}/etc/xdg/swaync/configSchema.json";
      }
    );
  };

  xdg.configFile."swaync/config.json" = lib.mkIf config.dotfiles.mutable {
    source = lib.mkForce (mkOutOfStoreSymlink "${filePath}/config.json");
  };
}
