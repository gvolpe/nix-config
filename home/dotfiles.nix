{ mutable }:

{ config, ... }:

{
  dotfiles = {
    inherit mutable;
    path = "${config.home.homeDirectory}/workspace/nix-config/home";
    sourceRoot = ./.;
  };
}
