{ lib, stdenv, fetchzip, breeze-icons, gtk3, gnome3, gnome-icon-theme, hicolor-icon-theme, mint-x-icons, pantheon, yaru-theme }:

stdenv.mkDerivation rec {
  pname   = "BeautyLine";
  version = "0.0.4";

  localVersion = "V3";

  src = fetchzip {
    name   = "${pname}-${localVersion}-${version}";
    url    = "https://github.com/gvolpe/BeautyLine/archive/refs/tags/${version}.tar.gz";
    sha256 = "sha256-L9I2DKxbn7T/L20eDwDiaClXGZP23NFAigc6c9rC3/Q=";
  };

  nativeBuildInputs = [ gtk3 ];

  propagatedBuildInputs = [
    breeze-icons
    gnome3.adwaita-icon-theme
    gnome-icon-theme
    hicolor-icon-theme
    mint-x-icons
    pantheon.elementary-icon-theme
    yaru-theme
  ];

  dontDropIconThemeCache = true;

  installPhase = ''
    mkdir -p $out/share/icons/${pname}
    cp -r ${pname}-${localVersion}/* $out/share/icons/${pname}/
    gtk-update-icon-cache $out/share/icons/${pname}
  '';

  meta = with lib; {
    description = "BeautyLine icon theme (v1.3 - Nov 2021)";
    homepage    = "https://www.gnome-look.org/p/1425426/";
    platforms   = platforms.linux;
    license     = licenses.gpl3Only;
    maintainers = with maintainers; [ gvolpe ];
  };
}
