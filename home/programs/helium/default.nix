{ config, lib, pkgs, ... }:

{
  home = {
    packages = [ pkgs.helium ];
    sessionVariables.BROWSER = "${lib.exe pkgs.helium}";
  };

  programs.helium-sync = {
    enable = true;
    settings = {
      configDir = "${config.xdg.configHome}/net.imput.helium";
      profile.name = "gvolpe";
      sync = {
        directory = "${config.home.homeDirectory}/workspace/helium-sync";
        enableGitOps = true;
      };
    };
  };

  xdg.mimeApps = {
    defaultApplications = {
      "application/x-extension-htm" = "helium.desktop";
      "application/x-extension-html" = "helium.desktop";
      "application/x-extension-shtml" = "helium.desktop";
      "application/x-extension-xht" = "helium.desktop";
      "application/x-extension-xhtml" = "helium.desktop";
      "application/xhtml+xml" = "helium.desktop";
      "text/html" = "helium.desktop";
      "text/xml" = "helium.desktop";
      "x-scheme-handler/chrome" = "helium.desktop";
      "x-scheme-handler/http" = "helium.desktop";
      "x-scheme-handler/https" = "helium.desktop";
    } // # unrelated to helium, needs a refactor
    {
      "application/pdf" = "org.pwmt.zathura-pdf-mupdf.desktop";
      "image/jpeg" = "org.gnome.Loupe.desktop";
      "image/png" = "org.gnome.Loupe.desktop";
      "x-scheme-handler/tg" = "org.telegram.desktop.desktop";
      "x-scheme-handler/tonsite" = "org.telegram.desktop.desktop";
    };

    enable = true;
  };
}
