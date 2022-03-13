{ pkgs, hdmiOn, ... }:

if hdmiOn then
  pkgs.symlinkJoin
  {
    name = "spotify";
    paths = [ pkgs.spotify ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/spotify --add-flags "-force-device-scale-factor=1.4"
    '';
  }
else
  pkgs.spotify
