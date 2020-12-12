# Configuration for the eDP display of the Tongfang laptop (default: HDMI-1)
{ config, lib, pkgs, stdenv, ... }:

let
  terminal = import ../programs/alacritty/default.nix { fontSize = 8; inherit pkgs; };
in
{
  imports = [
    ../home.nix
    terminal
  ];
}
