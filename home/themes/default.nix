{ pkgs, ... }:

let
  juno = pkgs.callPackage ./juno {};
in
{
  gtk = {
    enable = true;
    iconTheme = {
      name = "elementary-xfce";
      package = pkgs.elementary-xfce-icon-theme;
    };
    theme = {
      name = "Juno-ocean";
      package = juno;
    };
  };
}
