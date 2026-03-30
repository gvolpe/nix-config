{ config, pkgs, lib, ... }:

let
  cfg = config.software.defaults;
in
{
  options = {
    software = {
      defaults.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "install default list of packages";
      };
    };
  };

  config = lib.mkIf (cfg.enable && config.programs.home-manager.enable) {
    home.packages = with pkgs; [
      any-nix-shell # fish support for nix shell
      audacious # simple music player
      bazecor # configuration software for the dygma defy keyboard
      bottom # alternative to htop & ytop
      dig # dns command-line tool
      docker-compose # docker manager
      duf # disk usage/free utility
      eza # a better `ls`
      fd # "find" for files
      gimp # gnu image manipulation program
      hyperfine # command-line benchmarking tool
      insomnia # rest client with graphql support
      jmtpfs # mount mtp devices
      killall # kill processes by name
      libreoffice # office suite
      lnav # log file navigator on the terminal
      md-toc # # generate ToC in markdown files
      ncdu # disk space info (a better du)
      nitch # minimal system information fetch
      nix-output-monitor # nom: monitor nix commands
      nix-search # faster nix search client
      nyancat # the famous rainbow cat!
      ripgrep # fast grep
      socat # multipurpose relay (SOcket CAT)
      spotify # music player
      systemctl-tui # manage systemctl services and their logs
      telegram-desktop # telegram messaging client
      tree # display files in a tree view
      unzip # uncompress files
      vlc # media player
      xsel # clipboard support (also for neovim)
      yubioath-flutter # yubikey authenticator gui
      zip # compress files
    ] ++ (pkgs.sxm.scripts or [ ]);
  };
}
