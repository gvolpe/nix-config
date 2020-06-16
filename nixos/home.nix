{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;

  imports = [
    ./programs/git/default.nix
    ./programs/neovim/default.nix
  # ./program/tmux.nix
  ];

  xdg.enable = true;

  home.packages = with pkgs; [
    cachix # nix caching
    exa # a better `ls`
    fd # "find" for files
    ncdu # disk space info (a better du)
    prettyping # a nicer ping
    ripgrep # fast grep
    spotify # music source
    tldr # summary of a man page
    tree # display files in a tree view

    # fonts
    (
      nerdfonts.override {
        fonts = [
          "AnonymousPro"
        ];
      }
    )

    # git stuff
    gitAndTools.diff-so-fancy # git diff with colors
    gitAndTools.tig # diff and commit view
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

    #obs-studio = {
      #enable = true;
      #plugins = [];
    #};

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
