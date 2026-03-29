{ pkgs, lib, ... }:

let
  nerdFonts = with (pkgs.nerd-fonts); [
    jetbrains-mono
    iosevka
  ];

  fontPkgs = with pkgs; [
    font-awesome # awesome fonts
    material-design-icons # fonts with glyphs
  ] ++ nerdFonts;

  packages = with pkgs; [
    libnotify # notifications
    nemo # file manager
    networkmanagerapplet # network manager systray app
    wl-clipboard # clipboard support
    dig # dns command-line tool
    duf # disk usage/free utility
    eza # a better `ls`
    fd # "find" for files
    killall # kill processes by name
    ncdu # disk space info (a better du)
    ripgrep # fast grep
    socat # multipurpose relay (SOcket CAT)
    tree # display files in a tree view
    unzip # uncompress files
    xsel # clipboard support (also for neovim)
    zip # compress files
  ] ++ fontPkgs ++ (pkgs.sxm.scripts or [ ]);
in
{
  imports = [ ../../../home/shared ];

  home = {
    inherit packages;
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
}
