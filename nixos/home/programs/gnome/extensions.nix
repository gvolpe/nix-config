{ stdenv, fetchFromGitHub, glib, gettext }:

let
  mkGnomeExtension =
    def@{ pluginName
    , version
    , namePrefix ? "gnome-shell-extension-"
    , namePostfix ? "-${version}"
    , src
    , meta
    , ...
    }:
      stdenv.mkDerivation (
        def // {
          name = namePrefix + pluginName + namePostfix;
        }
      );
in
rec {
  inherit mkGnomeExtension;

  dash-to-dock = mkGnomeExtension {
    pluginName = "dash-to-dock";
    version    = "v68";

    src = fetchFromGitHub {
      owner  = "micheleg";
      repo   = "dash-to-dock";
      rev    = "e2a271a4c8930d59a4e660e7a8be0e8235681293";
      sha256 = "17a4fgl7p7f7hhwxjl0h4ay1z6r1myn59c1javgg9zfb2l3qf427";
    };

    makeFlags = [ "INSTALLBASE=$(out)/share/gnome-shell/extensions" ];

    nativeBuildInputs = [ glib gettext ];

    meta = with stdenv.lib; {
      description = "A dock for the Gnome Shell. This extension moves the dash out of the overview transforming it in a dock for an easier launching of applications and a faster switching between windows and desktops.";
      homepage    = https://github.com/micheleg/dash-to-dock;
      license     = licenses.gpl2;
      maintainers = [];
    };
  };

  timepp = mkGnomeExtension rec {
    pluginName = "timepp";
    version    = "2020.03.15";

    src = fetchFromGitHub {
      owner  = "zagortenay333";
      repo   = "timepp__gnome";
      rev    = "34ae477a51267cc1e85992a80cf85a1a7b7005c1";
      sha256 = "1v0xbrp0x5dwizscxh7h984pax4n92bj8iyw3qvjk27ynpxq8ag1";
    };

    uuid = "timepp@zagortenay333";

    installPhase = ''
      mkdir -p $out/share/gnome-shell/extensions/${uuid}
      cp -r . $out/share/gnome-shell/extensions/${uuid}
    '';

    meta = with stdenv.lib; {
      description = "A todo.txt manager, time tracker, timer, stopwatch, pomodoro, and alarms gnome-shell extension.";
      homepage    = https://github.com/zagortenay333/timepp__gnome;
      license     = licenses.gpl3;
      maintainers = [];
    };
  };

  topicons-plus = mkGnomeExtension {
    pluginName = "topicons-plus";
    version    = "v27";

    buildInputs = [ glib ];

    nativeBuildInputs = [ gettext ];

    makeFlags = [ "INSTALL_PATH=$(out)/share/gnome-shell/extensions" ];

    src = fetchFromGitHub {
      owner  = "phocean";
      repo   = "TopIcons-plus";
      rev    = "e51ac0a46ebcb6e71eaefc60b804668c57479a9e";
      sha256 = "1p3jlvs4zgnrvy8am7myivv4rnnshjp49kg87rd22qqyvcz51ykr";
    };

    meta = with stdenv.lib; {
      description = "An gnome-shell extension to put the icons back to the tray.";
      homepage    = https://github.com/phocean/TopIcons-plus;
      license     = licenses.gpl2;
      maintainers = [];
    };
  };

}
