{ pkgs, lib, ... }:

let
  associations = lib.secretManager {
    filepath = ../../secrets/mimeo-associations.txt;
    fileAction = builtins.readFile;
    encryptedSha256 = "a3542d7966833cef2482fcf3a1a678ef800d2908c216b7fcc13f36e86a6ffd6e";
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
