{ pkgs, ... }:

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
  };
}
