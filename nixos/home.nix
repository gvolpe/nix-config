{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home = {
    username = "gvolpe";
    homeDirectory = "/home/gvolpe";
    stateVersion = "20.09";
  };

  home.packages = with pkgs; [
    bat # a better `cat`
    cachix # nix caching
    exa # a better `ls`
    fd # "find" for files
    fish # better shell
    htop # interactive processes viewer
    ncdu # disk space info (a better du)
    ngrok # secure tunnels to localhost
    prettyping # a nicer ping
    ripgrep # fast grep
    spotify # music source
    tldr # summary of a man page
    tmux # terminal multiplexer and sessions
    tree # display files in a tree view

    # git stuff
    gitAndTools.diff-so-fancy # git diff with colors  
    gitAndTools.tig # diff and commit view
  ];

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
      enableBashIntegration = true;
    };

    fzf = {
      enable = true;
      enableBashIntegration = true;
    };

    git = {
      enable = true;
      aliases = {
        amend = "commit --amend -m";
        br = "branch";
        co = "checkout";
        st = "status";
        ls = "log --pretty=format:\"%C(yellow)%h%Cred%d\\\\ %Creset%s%Cblue\\\\ [%cn]\" --decorate";
        ll = "log --pretty=format:\"%C(yellow)%h%Cred%d\\\\ %Creset%s%Cblue\\\\ [%cn]\" --decorate --numstat";
        cm = "commit -m";
        ca = "commit -am";
        dc = "diff --cached";
      };
      extraConfig = {
        core = {
          pager = "diff-so-fancy | less --tabs=4 -RFX";
        };
        mergetool = {
          cmd = "nvim -f -c \"Gvdiffsplit!\" \"$MERGED\"";
          prompt = false;
        };
      };
      ignores = [
        "*.bloop"
        "*.metals"
        "*.metals.sbt"
        "*metals.sbt"
        "*.direnv"
      ];
      userEmail = "volpegabriel@gmail.com";
      userName = "gvolpe";
    };

    htop = {
      enable = true;
      sortDescending = true;
      sortKey = "PERCENT_CPU";
    };

    jq = {
      enable = true;
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
