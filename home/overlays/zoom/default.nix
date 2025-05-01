self: super:

{
  # see: https://github.com/NixOS/nixpkgs/pull/403064
  zoom-fhs = super.callPackage ./drv.nix {
    xdgDesktopPortalPkgs = with super; [
      xdg-desktop-portal
      xdg-desktop-portal-hyprland
    ];
  };
}
