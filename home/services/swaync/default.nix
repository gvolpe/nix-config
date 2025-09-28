{ config, lib, ... }:

let
  cfg = config.services.swaync;

  filePath = "${config.dotfiles.path}/services/swaync";

  jsonConfig = builtins.fromJSON (builtins.readFile ./config.json);

  style =
    if !config.dotfiles.mutable then ./style.css
    else config.lib.file.mkOutOfStoreSymlink "${filePath}/style.css";
in
{
  xdg.configFile."swaync/config.json" = lib.mkIf config.dotfiles.mutable {
    source = lib.mkForce (config.lib.file.mkOutOfStoreSymlink "${filePath}/config.json");
  };

  services.swaync = {
    inherit style;
    enable = true;
    settings = lib.mkIf (!config.dotfiles.mutable) (
      jsonConfig // {
        "$schema" = "${cfg.package}/etc/xdg/swaync/configSchema.json";
      }
    );
  };
}
