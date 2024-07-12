self: super:

{
  bazecor = super.applyPatches {
    src = super.bazecor;
    postPatch = ''
      rm lib/udev/rules.d/*dygma.rules
      ln -s --target-directory=lib/udev/rules.d ${./60-dygma.rules}
    '';
  };
}
