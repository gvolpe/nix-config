{ callPackage, ... }:

let
  bat-lvl = callPackage ./battery-level.nix { };
  gen-ssh-key = callPackage ./gen-ssh-key.nix { };
  hyprlax-toggle = callPackage ./hyprlax-toggle.nix { };
  kls = callPackage ./keyboard-layout-switch.nix { };
  mpvpaper-toggle = callPackage ./mpvpaper-toggle.nix { };
  satty-shot = callPackage ./screenshot.nix { };
  show-zombie-parents = callPackage ./show-zombie-parents.nix { };
  video = callPackage ./video.nix { };
in
{
  inherit bat-lvl gen-ssh-key
    hyprlax-toggle kls mpvpaper-toggle
    satty-shot show-zombie-parents video;
}
