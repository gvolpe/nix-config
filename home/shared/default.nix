{ pkgs, lib, ... }:

let
  username = "gvolpe";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
in
{
  programs.home-manager.enable = true;

  imports = lib.concatMap import [
    ../modules
    ../themes
    ./programs.nix
    ./services.nix
  ];

  xdg = {
    inherit configHome;
    enable = true;
  };

  home = {
    inherit username homeDirectory;

    changes-report.enable = true;

    sessionVariables = {
      BROWSER = "${lib.exe pkgs.firefox-beta}";
      DISPLAY = ":0";
      EDITOR = "nvim";
      # https://github.com/NixOS/nixpkgs/issues/24311#issuecomment-980477051
      GIT_ASKPASS = "";
    };
  };

  # garbage collection
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  # restart the corresponding systemd services on change
  systemd.user.startServices = "sd-switch";

  # notifications about home-manager news
  news.display = "silent";
}
