{ pkgs, lib, ... }:

let
  associations = lib.secretManager {
    filepath = ../../secrets/mimeo-associations.txt;
    fileAction = builtins.readFile;
    encryptedSha256 = "a3b60848f3951759c8fbffbdd834251c32c84fa7fa5272d065352793ae02401b";
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
