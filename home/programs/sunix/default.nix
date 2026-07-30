{ pkgs, ... }:

{
  programs.sunix = {
    enable = true;
    settings = {
      dixBinary = "${pkgs.dix}/bin/dix";
      flakeDir = "$HOME/workspace/sxm-flake";
      homeFlake = "niri-hdmi";
      nixosFlake = "aorus";
      showDemo = false;
    };
  };
}
