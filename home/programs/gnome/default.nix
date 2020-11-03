{ pkgs }:

let
  customGnome3Ext = pkgs.callPackage ./extensions.nix {};
in
  {
    gnomePkgs = with pkgs.gnome3; [
      # gnome3 apps
      eog      # image viewer
      evince   # pdf reader
      nautilus # file manager

      # desktop look & feel
      customGnome3Ext.dash-to-dock
      customGnome3Ext.timepp
      customGnome3Ext.topicons-plus
      pkgs.gnomeExtensions.clipboard-indicator
      pkgs.gnomeExtensions.sound-output-device-chooser
      gnome-tweak-tool
    ];
  }
