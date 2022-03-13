{ pkgs, hdmiOn, ... }:

# sync tool for mega.nz
if hdmiOn then
  pkgs.symlinkJoin
  {
    name = "megasync";
    paths = [ pkgs.megasync ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/megasync --prefix QT_SCALE_FACTOR : 1
    '';
  }
else
  pkgs.megasync
