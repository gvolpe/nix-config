{ lib, pkgs, ... }:

let
  inherit (builtins) attrNames map readFile readDir;

  src = pkgs.sources.niri-shaders;

  shaderNames = attrNames (
    lib.filterAttrs
      (name: type:
        type == "directory"
        && name != "glass-warp"
      )
      (readDir "${src}")
  );

  mkShaderFile = name:
    let
      shaderConfig = readFile "${src}/${name}/config";
      shaderOpen = readFile "${src}/${name}/open.glsl";
      shaderClose = readFile "${src}/${name}/close.glsl";
    in
    {
      name = "niri/config/shaders/${name}.kdl";

      value.text = ''
        animations {
            window-open {
                ${shaderConfig}

                custom-shader r"
                  ${shaderOpen}
                "
            }

            window-close {
                ${shaderConfig}

                custom-shader r"
                  ${shaderClose}
                "
            }
        }
      '';
    };
in
{
  xdg.configFile = lib.listToAttrs (map mkShaderFile shaderNames);
}
