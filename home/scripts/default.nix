{ callPackage, ... }:

let
  bat-lvl = callPackage ./battery-level.nix { };
  gen-ssh-key = callPackage ./gen-ssh-key.nix { };
  kls = callPackage ./keyboard-layout-switch.nix { };
  satty-shot = callPackage ./screenshot.nix { };
  show-zombie-parents = callPackage ./show-zombie-parents.nix { };
  video = callPackage ./video.nix { };
in
{
  inherit bat-lvl gen-ssh-key kls satty-shot show-zombie-parents video;
}
