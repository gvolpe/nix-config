{
  xresources.properties = {
    "Xft.dpi" = 180;
    "Xft.autohint" = 0;
    "Xft.hintstyle" = "hintfull";
    "Xft.hinting" = 1;
    "Xft.antialias" = 1;
    "Xft.rgba" = "rgb";
  };

  xsession = {
    enable = true;

    initExtra = ''
      setterm -blank 0 -powersave off -powerdown 0
      xset s off
    '';

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [
        hp.monad-logger
        hp.taffybar
        hp.xmonad-contrib
      ];
      config = ./config.hs;
    };
  };
}
