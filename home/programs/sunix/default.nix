{ pkgs, ... }:

{
  programs.sunix = {
    enable = true;
    settings = {
      dixBinary = "${pkgs.dix}/bin/dix";
      flakeDir = "$HOME/workspace/sxm-flake";
      homeFlake = "niri-desktop";
      nixosFlake = "aorus";
      showDemo = false;
    };
  };
}
