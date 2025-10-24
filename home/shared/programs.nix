let
  more = { pkgs, ... }: {
    programs = {
      bat.enable = true;

      broot = {
        enable = true;
        enableFishIntegration = true;
      };

      direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      fzf = {
        enable = true;
        enableFishIntegration = false; # broken
        defaultCommand = "fd --type file --follow"; # FZF_DEFAULT_COMMAND
        defaultOptions = [ "--height 20%" ]; # FZF_DEFAULT_OPTS
        fileWidgetCommand = "fd --type file --follow"; # FZF_CTRL_T_COMMAND
      };

      gpg.enable = true;

      htop = {
        enable = true;
        settings = {
          sort_direction = true;
          sort_key = "PERCENT_CPU";
        };
      };

      jq.enable = true;

      # generate index with: nix-index --filter-prefix '/bin/'
      nix-index-fork = {
        enable = true;
        enableFishIntegration = true;
        enableNixCommand = true;
        database = pkgs.nix-index-small-database;
      };
      # command-not-found only works with channels
      command-not-found.enable = false;

      obs-studio = {
        enable = false;
        plugins = [ ];
      };

      ssh = {
        enable = true;
        enableDefaultConfig = false;
        matchBlocks."*" = {
          forwardAgent = false;
          addKeysToAgent = "no";
          compression = false;
          serverAliveInterval = 0;
          serverAliveCountMax = 3;
          hashKnownHosts = false;
          userKnownHostsFile = "~/.ssh/known_hosts";
          controlMaster = "no";
          controlPath = "~/.ssh/master-%r@%n:%p";
          controlPersist = "no";
        };
      };

      zoxide = {
        enable = true;
        enableFishIntegration = true;
        options = [ ];
      };
    };
  };
in
[
  ../programs/dconf
  ../programs/git
  ../programs/firefox
  ../programs/fish
  ../programs/khal
  ../programs/mimeo
  ../programs/mpv
  ../programs/neofetch
  ../programs/neovim-ide
  ../programs/ngrok
  ../programs/yubikey
  ../programs/zathura
  more
]
