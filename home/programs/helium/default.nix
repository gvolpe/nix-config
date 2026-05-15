{ pkgs, lib, ... }:

{
  home = {
    packages = [ pkgs.helium ];
    sessionVariables.BROWSER = "${lib.exe pkgs.helium}";
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = "helium.desktop";
      "text/xml" = "helium.desktop";
      "x-scheme-handler/http" = "helium.desktop";
      "x-scheme-handler/https" = "helium.desktop";
      "x-scheme-handler/chrome" = "helium.desktop";
      "application/x-extension-htm" = "helium.desktop";
      "application/x-extension-html" = "helium.desktop";
      "application/x-extension-shtml" = "helium.desktop";
      "application/xhtml+xml" = "helium.desktop";
      "application/x-extension-xhtml" = "helium.desktop";
      "application/x-extension-xht" = "helium.desktop";
      # unrelated to helium, needs a refactor
      "x-scheme-handler/tg" = "org.telegram.desktop.desktop";
      "x-scheme-handler/tonsite" = "org.telegram.desktop.desktop";
      "image/png" = "org.gnome.Loupe.desktop";
      "image/jpeg" = "org.gnome.Loupe.desktop";
      "application/pdf" = "org.pwmt.zathura-pdf-mupdf.desktop";
    };
  };
}
