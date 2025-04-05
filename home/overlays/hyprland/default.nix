self: super:

let
  # see: https://github.com/hyprwm/Hyprland/issues/9518
  version = "0.46.2";
in
{
  hyprland = super.hyprland.overrideAttrs (old: {
    inherit version;

    src = super.fetchFromGitHub {
      owner = "hyprwm";
      repo = "hyprland";
      fetchSubmodules = true;
      rev = "v${version}";
      hash = "sha256-dj9dpVwpyTmUyVu4jtaIU39bHgVkoZjv6cgYfWyHc9E=";
    };
  });
}
