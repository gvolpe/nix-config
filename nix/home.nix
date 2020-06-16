{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  imports = [
    ./programs/git.nix
    ./programs/neovim.nix
    ./programs/tmux.nix
  ];

  home = {
    username = "gvolpe";
    homeDirectory = "/home/gvolpe";
    stateVersion = "20.09";
  };

  # notifications about home-manager news
  news.display = "silent";

  programs = {

    bat = {
      enable = true;
    };

    chromium = {
      enable = true;
      extensions = [
        "kklailfgofogmmdlhgmjgenehkjoioip" # google meet grid view
        "aapbdbdomjkkjkaonfhkkikfgjllcleb" # google translate
        "hdokiejnpimakedhajhdlcegeplioahd" # lastpass password manager
        "dbepggeogbaibhgnhhndojpepiihcmeb" # vimium
      ];
    };

    direnv = {
      enable = true;
      enableFishIntegration = true;
    };

    fish = {
      enable = true;
    };

    fzf = {
      enable = true;
      enableFishIntegration = true;
    };

    gpg = {
      enable = true;
    };

    htop = {
      enable = true;
      sortDescending = true;
      sortKey = "PERCENT_CPU";
    };

    jq = {
      enable = true;
    };

    obs-studio = {
      enable = true;
      plugins = [];
    };

    ssh = {
      enable = true;
    };

  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

}
