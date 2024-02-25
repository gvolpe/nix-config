{ pkgs, ... }:

let
  # xdg-utils as a runtime dependency: https://github.com/NixOS/nixpkgs/pull/181171
  xdg-mimeo = pkgs.xdg-utils.overrideAttrs (old: {
    postInstall = ''
      cp ${pkgs.mimeo}/bin/mimeo $out/bin/xdg-open
    '' + old.postInstall;
  });
in
{
  home.packages = [ pkgs.mimeo xdg-mimeo ];
  xdg.configFile."mimeo/associations.txt".text = pkgs.secrets.mimeoAssociations;
}
