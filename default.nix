{ pkgs ? import <nixpkgs> {} }:

{
  home = pkgs.callPackage ./home {};
  system = pkgs.callPackage ./system {};
}
