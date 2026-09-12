{ config, lib, pkgs, ... }:

let
  ngrok = pkgs.writeShellScriptBin "ngrok" ''
    set -euo pipefail

    for arg in "$@"; do
      case "$arg" in
        -h|--help|-v|--version|version|--authtoken|--authtoken=*)
          exec ${lib.getExe pkgs.ngrok} "$@"
          ;;
      esac
    done

    case "''${1:-}" in
      http|tcp|tls|start)
        token_file="${config.age.secrets.ngrok-token.path}"

        if [ ! -r "$token_file" ]; then
          echo "ngrok wrapper: missing agenix secret at $token_file" >&2
          exit 1
        fi

        authtoken="$(<"$token_file")"
        exec ${lib.getExe pkgs.ngrok} "$@" --authtoken "$authtoken"
        ;;
      *)
        exec ${lib.getExe pkgs.ngrok} "$@"
        ;;
    esac
  '';
in
{
  # secure tunneling to localhost
  home.packages = [ ngrok ];

  xdg.configFile."ngrok/ngrok.yml".text = lib.generators.toYAML { } {
    version = 2;
  };
}
