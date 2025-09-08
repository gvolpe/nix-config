{ lib, pkgs, ... }:

{
  # Super+Alt+L in the default setting (screen locker)
  programs.swaylock.enable = true;

  services.swayidle = {
    enable = true;

    events = [
      { event = "before-sleep"; command = "${lib.exe pkgs.swaylock} -fF"; }
      { event = "lock"; command = "lock"; }
    ];

    timeouts = [
      { timeout = 90; command = "${lib.exe pkgs.swaylock} -fF"; }
    ];
  };
}
