self: super:

{
  haskellPackages = with self.haskell.lib; super.haskellPackages.extend (
    hself: hsuper: {
      taffybar = hsuper.taffybar.overrideAttrs (
        drv: {
          nativeBuildInputs = drv.nativeBuildInputs or [] ++ [ super.pkgs.makeWrapper ];
          postInstall = drv.postInstall or "" + ''
            wrapProgram $out/bin/taffybar \
              --set GDK_PIXBUF_MODULE_FILE "${super.pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache"
          '';
        }
      );
    }
  );
}
