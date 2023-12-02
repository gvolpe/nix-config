{ pkgs, lib, ... }:

let
  associations = lib.secretManager {
    filepath = ../../secrets/mimeo-associations.txt;
    fileAction = lib.readFile;
    encryptedSha256 = "255dcce51ca73d2370bbdf1bf7e5c3f894da2b85049a2cec91e4afecb66e2087";
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
