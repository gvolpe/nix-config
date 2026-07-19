{ callPackage, gram-ext }:

let
  scala = callPackage ./scala.nix { };
in
with gram-ext; [
  bearded-icons
  catppuccin
  catppuccin-icons
  git-firefly
  night-owlz
  scala
]
