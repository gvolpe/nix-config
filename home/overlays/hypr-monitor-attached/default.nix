self: super:

let
  pname = "hyprland-monitor-attached";
  version = "0.1.6";
in
{
  hypr-monitor-attached = super.rustPlatform.buildRustPackage {
    inherit pname version;

    src = super.fetchFromGitHub {
      owner = "coffebar";
      repo = "hyprland-monitor-attached";
      rev = version;
      hash = "sha256-+bgOOm1B513COcWdUIJ/+GREQH5CR8/RNOcZVkjO2hI=";
    };

    cargoHash = "sha256-vQfDsP2Tc+Kj95wXIzPTlf6kRdBgdio0QkM9EJRjZjE=";

    meta = with super.lib; {
      description = "Run the user's script when you attach the monitor on Hyprland";
      homepage = "https://github.com/coffebar/hyprland-monitor-attached";
      license = licenses.mit;
      maintainers = with maintainers; [ gvolpe ];
      platforms = platforms.linux;
    };
  };
}
