{ lib, pkgs, ... }:

let
  inherit (builtins) attrNames map readFile readDir replaceStrings;

  src = pkgs.sources.nirimation;

  nirimationFiles = attrNames (
    lib.filterAttrs
      (name: type:
        type == "regular"
        && lib.hasSuffix ".kdl" name
      )
      (readDir "${src}/animations")
  );

  mkNirimationFile = filename:
    let
      source = "${src}/animations/${filename}";
    in
    {
      name = "niri/config/nirimation/${filename}";

      value =
        if filename == "roll-drop.kdl" then {
          text = replaceStrings
            [
              "window-open {\n        duration-ms 1000"
              "window-close {\n        duration-ms 1000"
            ]
            [
              "window-open {\n        duration-ms 500"
              "window-close {\n        duration-ms 700"
            ]
            (readFile source);
        }
        else { inherit source; };
    };
in
{
  xdg.configFile = lib.listToAttrs (map mkNirimationFile nirimationFiles);
}
