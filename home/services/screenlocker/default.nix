{ pkgs, ... }:

{
  services.screen-locker = {
    enable = true;
    inactiveInterval = 60;
    lockCmd = "${pkgs.betterlockscreen}/bin/betterlockscreen -l dim";
  };
}
