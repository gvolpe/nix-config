self: super:

let
  # see: https://github.com/hyprwm/Hyprland/issues/9518
  commit = "74d0f34cf396f4fc41ee98b000e7d0a4e277c066";
in
{
  hyprland = super.hyprland.overrideAttrs (old: {
    version = "main-${commit}";

    src = super.fetchFromGitHub {
      owner = "hyprwm";
      repo = "hyprland";
      fetchSubmodules = true;
      rev = "${commit}";
      hash = "sha256-8EM3SWF+GRv6CKW99UFQvC+/6586Upb5QSN2jb+Bjoo=";
    };
  });
}
