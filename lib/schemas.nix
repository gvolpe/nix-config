{ flake-schemas }:

let
  libFunction = output: name: shortDescription: {
    inherit shortDescription;
    evalChecks = {
      isFunction =
        builtins.hasAttr name output
        && builtins.isFunction (builtins.getAttr name output);
      isPresent = builtins.hasAttr name output;
    };
    what = "library function";
  };
in
{
  lib = {
    doc = ''
      Exports helper functions for constructing this flake's Home Manager and NixOS configurations.
    '';
    inventory = output:
      flake-schemas.lib.mkChildren {
        mkHome = libFunction output "mkHome" "Builds this flake's Home Manager configurations.";
        mkNixos = libFunction output "mkNixos" "Builds this flake's NixOS configurations.";
      };
    version = 1;
  };

  out = {
    doc = ''
      Exports custom attrsets like `pkgs` and `overlays` instances to be used externally.
    '';
    inventory = output:
      flake-schemas.lib.mkChildren (builtins.mapAttrs
        (_: _: {
          what = "custom instance to be used by consumers of this flake";
        })
        output);
    version = 1;
  };
}
