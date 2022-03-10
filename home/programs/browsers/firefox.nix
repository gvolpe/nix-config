{ pkgs, nur, hdmiOn, ... }:

let
  # disable the annoying floating icon with camera and mic when on a call
  disableWebRtcIndicator = ''
    #webrtcIndicator {
      display: none;
    }
  '';

  # ~/.mozilla/firefox/PROFILE_NAME/prefs.js | user.js
  sharedSettings = {
    "app.normandy.first_run" = false;
    "app.shield.optoutstudies.enabled" = false;

    # disable updates (pretty pointless with nix)
    "app.update.channel" = "default";

    "browser.contentblocking.category" = "standard"; # "strict"
    "browser.ctrlTab.recentlyUsedOrder" = false;

    "browser.download.useDownloadDir" = false;
    "browser.download.viewableInternally.typeWasRegistered.svg" = true;
    "browser.download.viewableInternally.typeWasRegistered.webp" = true;
    "browser.download.viewableInternally.typeWasRegistered.xml" = true;

    "browser.link.open_newwindow" = true;

    "browser.search.region" = "PL";
    "browser.search.widget.inNavBar" = true;

    "browser.shell.checkDefaultBrowser" = false;
    "browser.startup.homepage" = "https://nixos.org";
    "browser.tabs.loadInBackground" = true;
    "browser.urlbar.placeholderName" = "DuckDuckGo";
    "browser.urlbar.showSearchSuggestionsFirst" = false;

    "distribution.searchplugins.defaultLocale" = "en-US";

    "doh-rollout.balrog-migration-done" = true;
    "doh-rollout.doneFirstRun" = true;

    "dom.forms.autocomplete.formautofill" = false;

    "general.autoScroll" = true;
    "general.useragent.locale" = "en-US";

    "extensions.activeThemeID" = "firefox-alpenglow@mozilla.org";

    "extensions.extensions.activeThemeID" = "firefox-alpenglow@mozilla.org";
    "extensions.update.enabled" = false;
    "extensions.webcompat.enable_picture_in_picture_overrides" = true;
    "extensions.webcompat.enable_shims" = true;
    "extensions.webcompat.perform_injections" = true;
    "extensions.webcompat.perform_ua_overrides" = true;

    # DPI settings
    "layout.css.devPixelsPerPx" = if hdmiOn then "-1.0" else "1.25";

    "print.print_footerleft" = "";
    "print.print_footerright" = "";
    "print.print_headerleft" = "";
    "print.print_headerright" = "";

    "privacy.donottrackheader.enabled" = true;

    # Yubikey
    "security.webauth.u2f" = true;
    "security.webauth.webauthn" = true;
    "security.webauth.webauthn_enable_softtoken" = true;
    "security.webauth.webauthn_enable_usbtoken" = true;

    "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
  };
in
{
  programs.firefox = {
    enable = true;

    extensions = with nur.repos.rycee.firefox-addons; [
      bitwarden
      darkreader
      # auto-accepts cookies, use only with privacy-badger & ublock-origin
      i-dont-care-about-cookies
      languagetool
      link-cleaner
      privacy-badger
      to-deepl
      ublock-origin
      unpaywall
      vimium
    ];

    package = pkgs.firefox-beta-bin;

    profiles = {
      default = {
        id = 0;
        settings = sharedSettings;
        userChrome = disableWebRtcIndicator;
      };

      chatroulette = {
        id = 1;
        settings = sharedSettings;
        userChrome = disableWebRtcIndicator;
      };

      demo = {
        id = 2;
        settings = sharedSettings;
        userChrome = disableWebRtcIndicator;
      };
    };
  };
}
