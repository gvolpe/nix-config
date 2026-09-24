{ config, lib, pkgs, ... }:

{
  services.swaync = {
    enable = true;
    package = config.wrappers.swaync;
  };

  wrappers.swaync = lib.mkWrapper {
    basePackage = pkgs.swaynotificationcenter;
    prependFlags = [
      "--config"
      (config.dotfiles.make ./config.json)
      "--style"
      (config.dotfiles.make ./style.css)
    ];
  };
}
