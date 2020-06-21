{ config, pkgs, ... }:

let
  defaultPkgs = with pkgs; [
    cachix         # nix caching
    docker-compose # docker manager
    exa            # a better `ls`
    fd             # "find" for files
    k9s            # k8s pods manager
    ncdu           # disk space info (a better du)
    prettyping     # a nicer ping
    ripgrep        # fast grep
    rnix-lsp       # nix lsp server
    slack          # messaging client
    spotify        # music source
    terminator     # great terminal multiplexer
    tldr           # summary of a man page
    tree           # display files in a tree view

    # gnome3
    gnome3.gnome-tweak-tool

    # fonts
    (
      nerdfonts.override {
        fonts = [
          "AnonymousPro"
        ];
      }
    )
  ];

  gitPkgs = with pkgs; [
    gitAndTools.diff-so-fancy # git diff with colors
    gitAndTools.tig           # diff and commit view
  ];

  haskellPkgs = with pkgs.haskellPackages; [
    brittany      # code formatter
    cabal2nix     # convert cabal projects to nix
    cabal-install # package manager
    ghc           # compiler
    ghcide        # haskell IDE
    hoogle        # documentation
  ];
in
{
  programs.home-manager.enable = true;

  imports = [
    ./programs/git/default.nix
    #./programs/gnome/dconf.nix # only for fresh install for now
    ./programs/fish/default.nix
    ./programs/neovim/default.nix
    ./programs/sbt/default.nix
    ./programs/terminator/default.nix
    ./programs/tmux/default.nix
  ];

  xdg.enable = true;

  home.packages = defaultPkgs ++ gitPkgs ++ haskellPkgs;

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
        "gphhapmejobijbbhgpjhcjognlahblep" # gnome workspace grid extension
      ];
    };

    direnv = {
      enable = true;
      enableFishIntegration = true;
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

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
      enableSshSupport = true;
    };
  };

}
