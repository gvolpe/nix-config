final: prev:

let
  scripts = prev.callPackage ../../scripts { };
in
{
  inherit (scripts) bat-lvl gen-ssh-key helium-sync kls hyprlax-toggle mpvpaper-toggle satty-shot show-zombie-parents;
  video-scripts = scripts.video;
}
