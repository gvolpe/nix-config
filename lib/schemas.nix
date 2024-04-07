{ flake-schemas }:

{
  out = {
    version = 1;
    doc = ''
      Exports custom attrsets like `pkgs` and `overlays` instances to be used externally.
    '';
    inventory = output:
      flake-schemas.lib.mkChildren (builtins.mapAttrs
        (_: _: {
          what = "custom instance to be used by consumers of this flake";
        })
        output);
  };
}
