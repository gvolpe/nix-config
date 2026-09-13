{ config, ... }:

{
  programs.ngrok = {
    apiTokenPath = config.age.secrets.ngrok-token.path;
    enable = true;
    version = 2;
  };
}
