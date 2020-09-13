# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  dconf.settings = {
    "org/gnome/desktop/background" = {
      color-shading-type = "solid";
      picture-options    = "zoom";
      picture-uri        = "file:///home/gvolpe/Pictures/nixos.png";
      primary-color      = "#000000000000";
      secondary-color    = "#000000000000";
    };

    "org/gnome/desktop/input-sources" = {
      sources     = [ (mkTuple [ "xkb" "us" ]) ];
      xkb-options = [ " terminate:ctrl_alt_bksp " " lv3:ralt_switch " " caps:ctrl_modifier " ];
    };

    "org/gnome/desktop/interface" = {
      clock-show-weekday      = true;
      document-font-name      = "JetBrainsMono Nerd Font 11";
      enable-hot-corners      = false;
      font-name               = "JetBrainsMono Nerd Font 11";
      gtk-im-module           = "gtk-im-context-simple";
      gtk-theme               = "Adwaita-dark";
      icon-theme              = "Adwaita";
      monospace-font-name     = "JetBrainsMono Nerd Font 10";
      show-battery-percentage = true;
    };

    "org/gnome/desktop/peripherals/mouse" = {
      natural-scroll = false;
      speed          = -0.5;
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      tap-to-click                 = false;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/screensaver" = {
      picture-uri = "file:///home/gvolpe/Pictures/nixos.png";
    };

    "org/gnome/desktop/sound" = {
      allow-volume-above-100-percent = true;
      event-sounds                   = true;
    };

    "org/gnome/desktop/wm/keybindings" = {
      close                        = [ "<Super>w" ];
      switch-applications          = "@as []";
      switch-applications-backward = "@as []";
      switch-windows               = [ "<Alt>Tab" ];
      switch-windows-backward      = [ "<Shift><Alt>Tab" ];
    };

    " org/gnome/desktop/wm/preferences " = {
      button-layout   = "close,minimize,maximize:";
      titlebar-font   = "JetBrainsMono Nerd Font Mono 11";
      workspace-names = "@as []";
    };


    "org/gnome/nautilus/icon-view" = {
      default-zoom-level = "small";
    };

    "org/gnome/nautilus/preferences" = {
      default-folder-viewer   = "icon-view";
      search-filter-time-type = "last_modified";
      search-view             = "list-view";
    };

    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [ "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/" ];
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
      binding = "<Primary><Alt>t";
      command = "terminator";
      name    = "terminator";
    };

    "org/gnome/settings-daemon/plugins/power" = {
      sleep-inactive-ac-type      = "nothing";
      sleep-inactive-battery-type = "nothing";
    };

    "org/gnome/shell" = {
      command-history = [ "gnome-tweaks" ];
      disabled-extensions = [
        "apps-menu@gnome-shell-extensions.gcampax.github.com"
        "places-menu@gnome-shell-extensions.gcampax.github.com"
        "window-list@gnome-shell-extensions.gcampax.github.com"
        "windowsNavigator@gnome-shell-extensions.gcampax.github.com"
      ];
      enabled-extensions = [
        "horizontal-workspaces@gnome-shell-extensions.gcampax.github.com"
        "drive-menu@gnome-shell-extensions.gcampax.github.com"
        "screenshot-window-sizer@gnome-shell-extensions.gcampax.github.com"
        "workspace-indicator@gnome-shell-extensions.gcampax.github.com"
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "clipboard-indicator@tudmotu.com"
        "dash-to-dock@micxgx.gmail.com"
        "TopIcons@phocean.net"
        "sound-output-device-chooser@kgshank.net"
      ];
      favorite-apps = [
        "chromium-browser.desktop"
        "vivaldi.desktop"
        "spotify.desktop"
        "slack.desktop"
        "telegramdesktop.desktop"
        "org.gnome.Nautilus.desktop"
        "org.gnome.tweaks.desktop"
        "insomnia.desktop"
        "terminator.desktop"
      ];
    };

    "org/gnome/shell/extensions/dash-to-dock" = {
      background-opacity      = 0.80000000000000004;
      custom-theme-shrink     = false;
      dash-max-icon-size      = 48;
      dock-fixed              = false;
      dock-position           = "BOTTOM";
      extend-height           = false;
      force-straight-corner   = false;
      height-fraction         = 0.90000000000000002;
      hot-keys                = false;
      icon-size-fixed         = false;
      intellihide-mode        = "FOCUS_APPLICATION_WINDOWS";
      isolate-workspaces      = true;
      running-indicator-style = "DEFAULT";
      show-trash              = false;
      transparency-mode       = "DEFAULT";
    };

    "org/gnome/shell/extensions/desktop-icons" = {
      show-home  = false;
      show-trash = false;
    };

    "org/gnome/shell/extensions/user-theme" = {
      name = "";
    };

    "org/gnome/shell/app-switcher" = {
      current-workspace-only = true;
    };

    "org/gnome/system/location" = {
      enabled = false;
    };

    "org/gtk/settings/file-chooser" = {
      date-format            = "regular";
      location-mode          = "path-bar";
      show-hidden            = false;
      show-size-column       = true;
      show-type-column       = true;
      sidebar-width          = 189;
      sort-column            = "name";
      sort-directories-first = false;
      sort-order             = "ascending";
      type-format            = "category";
      window-position        = mkTuple [ 345 79 ];
      window-size            = mkTuple [ 1231 902 ];
    };

    "system/locale" = {
      region = "en_US.UTF-8";
    };

  };
}
