self: super:

{
  zoom-us = super.zoom-us.override {
    gnomeXdgDesktopPortalSupport = true;
  };

  zoom-us-hyprland = super.zoom-us.override {
    hyprlandXdgDesktopPortalSupport = true;
  };

  zoom-us-custom = super.zoom-us.override {
    targetPkgsFixed = with super; [
      xdg-desktop-portal
      xdg-desktop-portal-hyprland
    ];
  };
}
