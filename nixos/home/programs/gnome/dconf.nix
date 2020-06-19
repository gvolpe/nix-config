{
  dconf.settings = {
    "org/gnome/desktop/background" = {
      color-shading-type = "solid";
      picture-options = "zoom";
      picture-uri = "file:///home/gvolpe/Pictures/nixos.png";
      primary-color = "#000000000000";
      secondary-color = "#000000000000";
    };

    "org/gnome/desktop/input-sources" = {
      sources = "[('xkb', 'us')]";
      xkb-options = "['caps:ctrl_modifier']";
    };

    "org/gnome/desktop/interface" = {
      clock-show-weekday = true;
      enable-hot-corners = false;
      gtk-im-module = "gtk-im-context-simple";
      show-battery-percentage = true;
    };

    "org/gnome/desktop/peripherals/mouse" = {
      natural-scroll = false;
      speed = "-0.5";
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      tap-to-click = false;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/sound" = {
      allow-volume-above-100-percent = true;
      event-sounds = true;
    };

    "org/gnome/desktop/wm/keybindings" = {
      close = "['<Super>w']";
    };

    "org/gnome/desktop/wm/preferences" = {
      button-layout = "close,minimize,maximize:";
      num-workspaces = 4;
    };

    "org/gnome/eog/view" = {
      background-color = "rgb(0,0,0)";
      use-background-color = true;
    };

    "org/gnome/shell" = {
      command-history = "['r']";
      enabled-extensions = "['workspace-grid@mathematical.coffee.gmail.com', 'wsmatrix@martin.zurowietz.de', 'ubuntu-dock@ubuntu.com', 'user-theme@gnome-shell-extensions.gcampax.github.com']";
      favorite-apps = "['chromium.desktop', 'org.gnome.Nautilus.desktop', 'org.gnome.Software.desktop', 'org.gnome.gedit.desktop', 'spotify.desktop']";
    };

    "org/gnome/shell/extensions/dash-to-dock " = {
      dock-fixed = false;
      dock-position = " BOTTOM ";
    };

    "org/gnome/shell/extensions/desktop-icons " = {
      show-home = false;
      show-trash = false;
    };

    "org/gnome/shell/extensions/workspace-grid " = {
      num-columns = 2;
    };

    "org/gnome/shell/extensions/wsmatrix " = {
      num-columns = 2;
    };

    "org/gnome/system/location" = {
      enabled = false;
    };

    "system/locale" = {
      region = "en_US.UTF-8";
    };

  };
}
