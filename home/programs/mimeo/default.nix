{ pkgs, lib, ... }:

let
  associations = lib.secretManager {
    filepath = ../../secrets/mimeo-associations.txt;
    fileAction = builtins.readFile;
    encryptedSha256 = "7a8b8dc9072e03d229e77164db1fd7d3791b9d27f598d7e9c080e03b37ff4100";
    emptyValue = "";
  };

  # xdg-utils as a runtime dependency: https://github.com/NixOS/nixpkgs/pull/181171
  xdg-mimeo = pkgs.xdg-utils.overrideAttrs (old: {
    postInstall = ''
      cp ${pkgs.mimeo}/bin/mimeo $out/bin/xdg-open
    '' + old.postInstall;
  });
in
{
  home.packages = [ pkgs.mimeo xdg-mimeo ];
  xdg.configFile."mimeo/associations.txt".text = associations;
}
