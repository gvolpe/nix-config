[
  ({ pkgs, config, ... }:
    let
      cfg = config.gtk;
    in
    {
      gtk = {
        enable = true;
        iconTheme = {
          name = "BeautyLine";
          package = pkgs.beauty-line-icon-theme;
        };
        theme = {
          name = "Juno-ocean";
          package = pkgs.juno-theme;
        };
        gtk4 = {
          extraConfig = {
            gtk-application-prefer-dark-theme = true;
          };
          # the dark files are not copied by default, as not all themes have separate files
          # see: https://github.com/nix-community/home-manager/blob/afcedcf2c8e424d0465e823cf833eb3adebe1db7/modules/misc/gtk.nix#L238
          extraCss = ''
            @import url("file://${cfg.theme.package}/share/themes/${cfg.theme.name}/gtk-4.0/gtk-dark.css");
          '';
          theme = cfg.theme;
        };
      };
    })
]
