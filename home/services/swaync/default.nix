{ config, lib, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;

  cfg = config.services.swaync;
  filePath = "${config.dotfiles.path}/services/swaync";
  jsonConfig = builtins.fromJSON (builtins.readFile ./config.json);

  style =
    if !config.dotfiles.mutable then ./style.css
    else mkOutOfStoreSymlink "${filePath}/style.css";
in
{
  xdg.configFile."swaync/config.json" = lib.mkIf config.dotfiles.mutable {
    source = lib.mkForce (mkOutOfStoreSymlink "${filePath}/config.json");
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
