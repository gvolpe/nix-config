self: super:

{
  bazecor = super.callPackage ./drv.nix { };

  # TODO: bring it back on next nixpkgs update and remove drv.nix, as the 
  # derivation does not allow to simply override the version and src
  bazecor-old = super.applyPatches {
    src = super.bazecor;
    postPatch = ''
      rm lib/udev/rules.d/*dygma.rules
      ln -s --target-directory=lib/udev/rules.d ${./60-dygma.rules}
    '';
  };
}
