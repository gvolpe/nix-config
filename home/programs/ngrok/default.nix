{ pkgs, lib, ... }:

let
  ngrokConfig = lib.generators.toYAML { } {
    version = 2;
    authtoken = pkgs.secrets.ngrokToken;
  };
in
{
  # secure tunneling to localhost
  home.packages = [ pkgs.ngrok ];

  xdg.configFile."ngrok/ngrok.yml".text = ngrokConfig;
}
