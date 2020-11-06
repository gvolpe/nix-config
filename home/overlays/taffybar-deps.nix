self: super:

{
  haskellPackages = with self.haskell.lib; super.haskellPackages.extend (
    hself: hsuper: {
      gi-cairo-connector = markUnbroken (hsuper.gi-cairo-connector);
      gi-cairo-render = markUnbroken (
        overrideCabal (hsuper.gi-cairo-render) (
          drv: {
            src = self.fetchFromGitHub {
              owner = "thestr4ng3r";
              repo = "gi-cairo-render";
              rev = "8727c43cdf91aeedffc9cb4c5575f56660a86399";
              sha256 = "16kqh2ck0dad1l4m6q9xs5jqj9q0vgpqrzb2dc90jk8xwslmmhxd";
            };
            editedCabalFile = null;
            postUnpack = ''
              mv source all
              mv all/gi-cairo-render source
            '';
          }
        )
      );
      gi-dbusmenu = markUnbroken (hself.gi-dbusmenu_0_4_8);
      gi-dbusmenugtk3 = markUnbroken (hself.gi-dbusmenugtk3_0_4_9);
      gi-gdk = hself.gi-gdk_3_0_23;
      gi-gdkx11 = markUnbroken (
        overrideSrc hsuper.gi-gdkx11 {
          src = self.fetchurl {
            url = "https://hackage.haskell.org/package/gi-gdkx11-3.0.10/gi-gdkx11-3.0.10.tar.gz";
            sha256 = "0kfn4l5jqhllz514zw5cxf7181ybb5c11r680nwhr99b97yy0q9f";
          };
          version = "3.0.10";
        }
      );
      gi-gtk-hs = markUnbroken (hself.gi-gtk-hs_0_3_9);
      gi-xlib = markUnbroken (hself.gi-xlib_2_0_9);
      taffybar = markUnbroken (
        appendPatch hsuper.taffybar (
          self.fetchpatch {
            url = "https://github.com/taffybar/taffybar/pull/494/commits/a7443324a549617f04d49c6dfeaf53f945dc2b98.patch";
            sha256 = "0prskimfpapgncwc8si51lf0zxkkdghn33y3ysjky9a82dsbhcqi";
          }
        )
      );
      gtk-sni-tray = markUnbroken (hsuper.gtk-sni-tray);
      gtk-strut = markUnbroken (hsuper.gtk-strut);
    }
  );
}
