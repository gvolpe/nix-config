{ pkgs, ... }:

{
  services.screen-locker = {
    enable = true;
    inactiveInterval = 30;
    lockCmd = "${pkgs.multilockscreen}/bin/multilockscreen -l dim";
    xautolockExtraOptions = [
      "Xautolock.killer: systemctl suspend"
    ];
  };
}
