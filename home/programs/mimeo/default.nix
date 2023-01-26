{ pkgs, lib, ... }:

let
  associations = lib.secretManager {
    filepath = ../../secrets/mimeo-associations.txt;
    fileAction = builtins.readFile;
    encryptedSha256 = "7a8b8dc9072e03d229e77164db1fd7d3791b9d27f598d7e9c080e03b37ff4100";
    emptyValue = "";
  };
in
{
  xdg.configFile."mimeo/associations.txt".text = associations;
}
