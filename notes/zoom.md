# zoom-us

`zoom-us` workaround for the "unable to find desktop portals error": https://github.com/NixOS/nixpkgs/issues/359533

Add to NixOS config:

```nix
{
  systemd.user.tmpfiles.rules = [
    "L+ /usr/share/xdg-desktop-portal/portals - - - - /run/current-system/sw/share/xdg-desktop-portal/portals "
    "L+ /usr/libexec/xdg-desktop-portal-gtk - - - - ${pkgs.xdg-desktop-portal-gtk}/libexec/xdg-desktop-portal-gtk "
    "L+ /usr/libexec/xdg-desktop-portal-hyprland - - - - ${pkgs.xdg-desktop-portal-hyprland}/libexec/xdg-desktop-portal-hyprland "
    "L+ /usr/libexec/xdg-desktop-portal - - - - ${pkgs.xdg-desktop-portal}/libexec/xdg-desktop-portal "
  ];
}
```
