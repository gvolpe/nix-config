{ pkgs ? import <nixpkgs> {} }:

pkgs.callPackage ./system/configuration.nix {}
