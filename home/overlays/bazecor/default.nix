final: prev:

{
  bazecor = prev.applyPatches {
    src = prev.bazecor;
    postPatch = ''
      rm lib/udev/rules.d/*dygma.rules
      ln -s --target-directory=lib/udev/rules.d ${./60-dygma.rules}
    '';
  };
}
