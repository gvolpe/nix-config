{ pkgs, ... }:

{
  programs.sunix = {
    enable = true;
    settings = {
      dixBinary = "${pkgs.dix}/bin/dix";
      enableBuildPlans = true;
      flakeDir = "$HOME/workspace/sxm-flake";
      homeFlake = "niri-desktop";
      nixosFlake = "aorus";
      showDemo = false;
      nixpkgsInput = "nix-config/nixpkgs";
      nixpkgsUrl = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.zst";
    };
  };
}
