{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

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

    # TODO: Migrate init.vim here
    neovim = {
      enable = true;
      extraConfig = ""; # init.vim content here
      extraPythonPackages = "ps: []";
      plugins = [];
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
      withNodeJs = false; # true if coc.nvim is needed
      withPython = true; # for plugins
      withPython3 = true; # for plugins
    };

    obs-studio = {
      enable = true;
      plugins = [];
    };

    ssh = {
      enable = true;
    };

    # TODO: migrate tmux config
    tmux = {
      enable = true;
      extraConfig = "";
      keyMode = "vi";
      plugins = with pkgs; [
        tmuxPlugins.cpu
        tmuxPlugins.nordTmux
        {
          plugin = tmuxPlugins.resurrect;
          extraConfig = "set -g @resurrect-strategy-nvim 'session'";
        }
        {
          plugin = tmuxPlugins.continuum;
          extraConfig = ''
            set -g @continuum-restore 'on'
            set -g @continuum-save-interval '60' # minutes
          '';
        }
      ];
      shortcut = "a";
    };

  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

}
