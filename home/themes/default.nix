{ pkgs, ... }:

let
  beauty-line = pkgs.callPackage ./beauty-line {};
  juno        = pkgs.callPackage ./juno {};
in
{
  gtk = {
    enable = true;
    iconTheme = {
      name = "BeautyLine";
      package = beauty-line;
    };
    theme = {
      name = "Juno-ocean";
      package = juno;
    };
  };
}
