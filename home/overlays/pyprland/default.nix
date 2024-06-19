self: super:

let
  version = "2.3.8";
in
{
  pyprland = super.pyprland.overrideAttrs (old: {
    inherit version;

    src = super.fetchFromGitHub {
      owner = "hyprland-community";
      repo = "pyprland";
      rev = version;
      hash = "sha256-0YUI2/gJmBoummiHGpq2p2sT25SwCdnsRwfGK2pcm4s=";
    };
  });
}
