final: prev:

let
  toml = fromTOML (builtins.readFile "${prev.sources.hypr-monitor-attached}/Cargo.toml");
in
{
  hypr-monitor-attached = prev.rustPlatform.buildRustPackage {
    pname = "hyprland-monitor-attached";
    version = toml.package.version;

    src = prev.sources.hypr-monitor-attached;
    cargoHash = "sha256-pBg5R7k3xEE1EoSdLO4jmibTnGE+ndZnkWeMO+UXN6Q=";

    meta = with prev.lib; {
      description = "Run the user's script when you attach the monitor on Hyprland";
      homepage = "https://github.com/coffebar/hyprland-monitor-attached";
      license = licenses.mit;
      maintainers = with maintainers; [ gvolpe ];
      platforms = platforms.linux;
    };
  };
}
