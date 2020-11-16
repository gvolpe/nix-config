## Wayland support (WIP)

Wayland works great in Gnome3, except it doesn't fully support screen-sharing in many applications I need for work such as Slack and Google Meet. However, here's a workaround I tried a while ago to get it working before giving up and switching back to Xorg.

This goes in `configuration.nix`:

```nix
let
  waylandPkg = builtins.fetchTarball {
    url = "https://github.com/colemickens/nixpkgs-wayland/archive/master.tar.gz";
    sha256 = "14h4gnljrr0mhxkfcdwc6nm3ysh8jjy9wq6b0iba1ppvyw59vdws";
  };
  waylandOverlay = import waylandPkg;

  chromiumPkg = builtins.fetchTarball {
    url    = "https://github.com/colemickens/nixpkgs-chromium/archive/master.tar.gz";
    sha256 = "0d5gmcnalh3x154mg40cx70d48a9nvn5x8kkcp2xxp0cha6hqh96";
  };
  chromium = import chromiumPkg;
in {
  nix = {
    binaryCachePublicKeys = [ "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA=" ];
    binaryCaches = [ "https://nixpkgs-wayland.cachix.org" ];
  };

  # overlays
  nixpkgs.overlays = [ waylandOverlay ];

  environment.systemPackages = with pkgs; [ wayvnc ];

  # Enable screen-sharing for Wayland (Gnome3)
  services.pipewire.enable = true;
  services.xserver.displayManager.gdm.wayland = true;
}
```
