final: prev:

let
  scripts = prev.callPackage ../../scripts { };
in
{
  inherit (scripts) bat-lvl gen-ssh-key kls satty-shot show-zombie-parents;
  video-scripts = scripts.video;
}
