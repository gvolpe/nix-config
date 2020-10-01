# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  dconf.settings = {
    "org/gnome/desktop/wm/preferences" = {
      button-layout = "close,minimize,maximize:";
      titlebar-font = "JetBrainsMono Nerd Font Mono 11";
      workspace-names = "@as []";
    };

    "org/gnome/control-center" = {
      last-panel = "mouse";
    };

    "org/gnome/desktop/app-folders" = {
      folder-children = [ "Utilities" "YaST" ];
    };

    "org/gnome/desktop/app-folders/folders/Utilities" = {
      apps = [ "gnome-abrt.desktop" "gnome-system-log.desktop" "gnome-system-monitor.desktop" "gucharmap.desktop" "nm-connection-editor.desktop" "org.gnome.baobab.desktop" "org.gnome.Calculator.desktop" "org.gnome.DejaDup.desktop" "org.gnome.Dictionary.desktop" "org.gnome.DiskUtility.desktop" "org.gnome.eog.desktop" "org.gnome.Evince.desktop" "org.gnome.FileRoller.desktop" "org.gnome.fonts.desktop" "org.gnome.Screenshot.desktop" "org.gnome.seahorse.Application.desktop" "org.gnome.Terminal.desktop" "org.gnome.tweaks.desktop" "org.gnome.Usage.desktop" "simple-scan.desktop" "vinagre.desktop" "yelp.desktop" ];
      categories = [ "X-GNOME-Utilities" ];
      name = "X-GNOME-Utilities.directory";
      translate = true;
    };

    "org/gnome/desktop/app-folders/folders/YaST" = {
      categories = [ "X-SuSE-YaST" ];
      name = "suse-yast.directory";
      translate = true;
    };

    "org/gnome/desktop/background" = {
      color-shading-type = "solid";
      picture-options = "zoom";
      picture-uri = "file:///home/gvolpe/Pictures/nixos.png";
      primary-color = "#000000000000";
      secondary-color = "#000000000000";
    };

    "org/gnome/desktop/input-sources" = {
      current = "uint32 0";
      sources = [ (mkTuple [ "xkb" "us" ]) ];
      xkb-options = [ "terminate:ctrl_alt_bksp" "lv3:ralt_switch" "caps:ctrl_modifier" ];
    };

    "org/gnome/desktop/interface" = {
      clock-show-weekday = true;
      document-font-name = "JetBrainsMono Nerd Font 11";
      enable-hot-corners = false;
      font-name = "JetBrainsMono Nerd Font 11";
      gtk-im-module = "gtk-im-context-simple";
      gtk-theme = "Adwaita-dark";
      icon-theme = "Adwaita";
      monospace-font-name = "JetBrainsMono Nerd Font 10";
      show-battery-percentage = true;
    };

    "org/gnome/desktop/notifications" = {
      application-children = [ "telegramdesktop" "slack" ];
    };

    "org/gnome/desktop/notifications/application/slack" = {
      application-id = "slack.desktop";
    };

    "org/gnome/desktop/notifications/application/telegramdesktop" = {
      application-id = "telegramdesktop.desktop";
    };

    "org/gnome/desktop/peripherals/mouse" = {
      natural-scroll = false;
      speed = -0.5;
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      tap-to-click = false;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/privacy" = {
      report-technical-problems = true;
    };

    "org/gnome/desktop/screensaver" = {
      picture-uri = "file:///home/gvolpe/Pictures/nixos.png";
    };

    "org/gnome/desktop/sound" = {
      allow-volume-above-100-percent = true;
      event-sounds = true;
    };

    "org/gnome/desktop/wm/keybindings" = {
      close = [ "<Super>w" ];
      switch-applications = "@as []";
      switch-applications-backward = "@as []";
      switch-windows = [ "<Alt>Tab" ];
      switch-windows-backward = [ "<Shift><Alt>Tab" ];
    };

    "org/gnome/evolution-data-server" = {
      migrated = true;
      network-monitor-gio-name = "";
    };

    "org/gnome/gnome-screenshot" = {
      border-effect = "none";
      delay = 0;
      include-border = true;
      include-pointer = false;
      last-save-directory = "file:///home/gvolpe/Pictures";
    };

    "org/gnome/mutter" = {
      attach-modal-dialogs = true;
      dynamic-workspaces = true;
      edge-tiling = true;
      focus-change-on-pointer-rest = true;
      workspaces-only-on-primary = true;
    };

    "org/gnome/nautilus/icon-view" = {
      default-zoom-level = "small";
    };

    "org/gnome/nautilus/preferences" = {
      default-folder-viewer = "icon-view";
      search-filter-time-type = "last_modified";
      search-view = "list-view";
    };

    "org/gnome/nautilus/window-state" = {
      initial-size = mkTuple [ 890 550 ];
      maximized = false;
      sidebar-width = 189;
    };

    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [ "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/" ];
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
      binding = "<Primary><Alt>t";
      command = "terminator";
      name = "terminator";
    };

    "org/gnome/settings-daemon/plugins/power" = {
      sleep-inactive-ac-type = "nothing";
      sleep-inactive-battery-type = "nothing";
    };

    "org/gnome/shell" = {
      command-history = [ "gnome-tweaks" ];
      disabled-extensions = [ "apps-menu@gnome-shell-extensions.gcampax.github.com" "places-menu@gnome-shell-extensions.gcampax.github.com" "window-list@gnome-shell-extensions.gcampax.github.com" "windowsNavigator@gnome-shell-extensions.gcampax.github.com" ];
      enabled-extensions = [ "horizontal-workspaces@gnome-shell-extensions.gcampax.github.com" "drive-menu@gnome-shell-extensions.gcampax.github.com" "screenshot-window-sizer@gnome-shell-extensions.gcampax.github.com" "workspace-indicator@gnome-shell-extensions.gcampax.github.com" "user-theme@gnome-shell-extensions.gcampax.github.com" "clipboard-indicator@tudmotu.com" "dash-to-dock@micxgx.gmail.com" "TopIcons@phocean.net" "sound-output-device-chooser@kgshank.net" ];
      favorite-apps = [ "chromium-browser.desktop" "brave-browser.desktop" "spotify.desktop" "slack.desktop" "telegramdesktop.desktop" "org.gnome.Nautilus.desktop" "org.gnome.tweaks.desktop" "insomnia.desktop" "terminator.desktop" ];
    };

    "org/gnome/shell/app-switcher" = {
      current-workspace-only = true;
    };

    "org/gnome/shell/extensions/dash-to-dock" = {
      background-opacity = 0.8;
      custom-theme-shrink = false;
      dash-max-icon-size = 48;
      dock-fixed = false;
      dock-position = "BOTTOM";
      extend-height = false;
      force-straight-corner = false;
      height-fraction = 0.9;
      hot-keys = false;
      icon-size-fixed = false;
      intellihide-mode = "FOCUS_APPLICATION_WINDOWS";
      isolate-workspaces = true;
      running-indicator-style = "DEFAULT";
      show-trash = false;
      transparency-mode = "DEFAULT";
    };

    "org/gnome/shell/extensions/desktop-icons" = {
      show-home = false;
      show-trash = false;
    };

    "org/gnome/shell/extensions/user-theme" = {
      name = "";
    };

    "org/gnome/shell/world-clocks" = {
      locations = "@av []";
    };

    "org/gnome/software" = {
      check-timestamp = "int64 1600351478";
    };

    "org/gnome/system/location" = {
      enabled = false;
    };

    "org/gnome/terminal/legacy" = {
      theme-variant = "dark";
    };

    "org/gtk/settings/file-chooser" = {
      date-format = "regular";
      location-mode = "path-bar";
      show-hidden = false;
      show-size-column = true;
      show-type-column = true;
      sidebar-width = 189;
      sort-column = "name";
      sort-directories-first = false;
      sort-order = "ascending";
      type-format = "category";
      window-position = mkTuple [ 345 79 ];
      window-size = mkTuple [ 1231 902 ];
    };

    "system/locale" = {
      region = "en_US.UTF-8";
    };

  };
}
