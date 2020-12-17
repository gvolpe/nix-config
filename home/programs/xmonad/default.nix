let
  extra = ''
    setterm -blank 0 -powersave off -powerdown 0
    xset s off
    xcape -e "Hyper_L=Tab;Hyper_R=backslash"
    xrandr --output HDMI-A-0 --mode 3840x2160 --rate 30.00
  '';

  polybarOpts = ''
    nitrogen --restore &
    pasystray &
    blueman-applet &
    nm-applet --sm-disable --indicator &
  '';

  taffybarOpts = ''
    status-notifier-watcher &
    nitrogen --restore &
    taffybar &
    pasystray &
    blueman-applet &
    nm-applet --sm-disable --indicator &
  '';
in
{
  xresources.properties = {
    "Xft.dpi" = 180;
    "Xft.autohint" = 0;
    "Xft.hintstyle" = "hintfull";
    "Xft.hinting" = 1;
    "Xft.antialias" = 1;
    "Xft.rgba" = "rgb";
    "Xcursor*theme" = "Vanilla-DMZ-AA";
    "Xcursor*size" = 24;
  };

  xsession = {
    enable = true;

    initExtra = extra + polybarOpts;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [
        hp.dbus
        hp.monad-logger
      ];
      config = ./config.hs;
    };
  };
}
