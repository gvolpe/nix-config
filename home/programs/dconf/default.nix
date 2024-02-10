# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "org/gnome/nautilus/icon-view" = {
      default-zoom-level = "standard";
    };

    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
    };

    "org/gnome/nautilus/preferences" = {
      default-folder-viewer = "icon-view";
      default-sort-order = "type";
      migrated-gtk-settings = true;
      search-filter-time-type = "last_modified";
      search-view = "list-view";
    };

    "org/gnome/nautilus/window-state" = {
      initial-size = mkTuple [ 500 400 ];
      maximized = false;
      sidebar-width = 200;
      start-with-sidebar = true;
    };

    "org/gtk/gtk4/settings/file-chooser" = {
      date-format = "regular";
      location-mode = "path-bar";
      show-hidden = false;
      show-size-column = true;
      show-type-column = true;
      sidebar-width = 263;
      sort-column = "name";
      sort-directories-first = true;
      sort-order = "ascending";
      type-format = "category";
      window-size = mkTuple [ 100 100 ];
    };

    "org/gtk/settings/file-chooser" = {
      window-position = mkTuple [ (-1) (-1) ];
      window-size = mkTuple [ 300 100 ];
    };
  };
}
