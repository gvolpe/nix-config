# Configuration for the HDMI-1 display monitor
{ config, lib, pkgs, stdenv, ... }:

let
  terminal = import ../programs/alacritty/default.nix { fontSize = 10; inherit pkgs; };
in
{
  imports = [
    ../home.nix
    terminal
  ];
}
