{ config, lib, pkgs, ... }:

let
  cfg = config.programs.helium-sync;

  helium-sync = pkgs.callPackage ./drv.nix { inherit (cfg) settings; };
in
{
  config = lib.mkIf cfg.enable {
    home.packages = [ helium-sync ];
  };

  options = {
    programs.helium-sync = {
      enable = lib.mkEnableOption "Enable the helium-sync program (syncing helium browser state via a git repo)";

      settings = {
        configDir = lib.mkOption {
          default = "${config.xdg.configHome}/net.imput.helium";
          description = "The directory where the helium browser state lives";
          type = lib.types.str;
        };

        profile = {
          id = lib.mkOption {
            default = "Default";
            description = "The profile identifier";
            type = lib.types.str;
          };

          name = lib.mkOption {
            description = "The profile name to sync";
            example = "gvolpe";
            type = lib.types.str;
          };
        };

        sync = {
          directory = lib.mkOption {
            description = "The directory of the git repository where state is sync'd";
            example = "${config.home.homeDirectory}/workspace/helium-sync";
            type = lib.types.str;
          };

          enableGitOps = lib.mkOption {
            default = true;
            description = "Whether pull/push commands should also run git pull/push in the sync directory";
            type = lib.types.bool;
          };
        };
      };
    };
  };
}
