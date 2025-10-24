self: super:

let
  toml = fromTOML (builtins.readFile "${super.xargs.diskonaut-src}/Cargo.toml");
in
{
  hypr-monitor-attached = super.rustPlatform.buildRustPackage {
    pname = "hyprland-monitor-attached";
    version = toml.package.version;

    src = super.xargs.hypr-monitor-attached-src;
    cargoHash = "sha256-pBg5R7k3xEE1EoSdLO4jmibTnGE+ndZnkWeMO+UXN6Q=";

    meta = with super.lib; {
      description = "Run the user's script when you attach the monitor on Hyprland";
      homepage = "https://github.com/coffebar/hyprland-monitor-attached";
      license = licenses.mit;
      maintainers = with maintainers; [ gvolpe ];
      platforms = platforms.linux;
    };
  };
}
