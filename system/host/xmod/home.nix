{ pkgs, lib, ... }:

{
  imports = [ ../../../home/shared ];

  home = {
    packages = with pkgs; [
      any-nix-shell
      dig # dns command-line tool
      duf # disk usage/free utility
      eza # a better `ls`
      fd # "find" for files
      killall # kill processes by name
      ripgrep # fast grep
      tree # display files in a tree view
      unzip # uncompress files
      zip # compress files
    ];

    stateVersion = "23.05";

    changes-report.enable = lib.mkForce false;

    sessionVariables = {
      NIXOS_OZONE_WL = 1;
      SHELL = "${lib.exe pkgs.fish}";
      MOZ_ENABLE_WAYLAND = 1;
      ELECTRON_OZONE_PLATFORM_HINT = "auto";
    };
  };

  fonts.fontconfig.enable = true;

  software.defaults.enable = lib.mkForce false;
  services.udiskie.enable = lib.mkForce false;
}
