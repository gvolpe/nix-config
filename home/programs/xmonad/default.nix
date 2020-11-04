{
  xsession = {
    enable = true;

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
