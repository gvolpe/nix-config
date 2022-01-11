self: super:

{
  ranger = super.ranger.overrideAttrs (
    old: rec {
      # set ueberzug as the default method for previewing images on the terminal
      preConfigure = old.preConfigure + ''
        substituteInPlace ranger/ext/img_display.py \
          --replace "Popen(['ueberzug'" "Popen(['${self.ueberzug}/bin/ueberzug'"
        substituteInPlace ranger/config/rc.conf \
          --replace "set preview_images_method w3m" "set preview_images_method ueberzug"
      '';
    }
  );
}
