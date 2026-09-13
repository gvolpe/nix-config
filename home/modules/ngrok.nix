{ config, lib, pkgs, ... }:

let
  cfg = config.programs.ngrok;

  ngrok-wrapper = pkgs.writeShellScriptBin "ngrok" ''
    set -euo pipefail

    for arg in "$@"; do
      case "$arg" in
        -h|--help|-v|--version|version|--authtoken|--authtoken=*)
          exec ${lib.getExe cfg.package} "$@"
          ;;
      esac
    done

    case "''${1:-}" in
      http|tcp|tls|start)
        token_file="${cfg.apiTokenPath}"

        if [ ! -r "$token_file" ]; then
          echo "ngrok wrapper: missing agenix secret at $token_file" >&2
          exit 1
        fi

        authtoken="$(<"$token_file")"
        exec ${lib.getExe cfg.package} "$@" --authtoken "$authtoken"
        ;;
      *)
        exec ${lib.getExe cfg.package} "$@"
        ;;
    esac
  '';
in
{
  config = lib.mkIf cfg.enable {
    home.packages = [ ngrok-wrapper ];

    xdg.configFile."ngrok/ngrok.yml".text = lib.generators.toYAML { } {
      version = cfg.version;
    };
  };

  options = {
    programs.ngrok = {
      apiTokenPath = lib.mkOption {
        description = "The ngrok API token path (e.g. /run/agenix/ngrok-token)";
        type = lib.types.str;
      };

      enable = lib.mkEnableOption "Enable the ngrok program (secure tunneling to localhost)";

      package = lib.mkOption {
        default = pkgs.ngrok;
        description = "The ngrok package derivation";
        type = lib.types.package;
      };

      version = lib.mkOption {
        default = 2;
        description = "The configured version";
        type = lib.types.int;
      };
    };
  };
}
