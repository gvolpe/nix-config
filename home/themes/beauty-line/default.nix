{ lib, stdenv, fetchzip, breeze-icons, gtk3, gnome3, gnome-icon-theme, hicolor-icon-theme, mint-x-icons, pantheon, yaru-theme }:

stdenv.mkDerivation rec {
  pname   = "BeautyLine";
  version = "0.0.3";

  # to test locally
  #src = /home/gvolpe/workspace/BeautyLine/BeautyLine-V2;

  src = fetchzip {
    name   = "${pname}-V1-${version}";
    url    = "https://github.com/gvolpe/BeautyLine/releases/download/${version}/BeautyLine-V2.tar.xz";
    sha256 = "037w51wgi8is2hm86an4vjjn4mwmfxv9n3xpr394j0x8gcij64lg";
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
    cp -r * $out/share/icons/${pname}/
    gtk-update-icon-cache $out/share/icons/${pname}
  '';

  meta = with lib; {
    description = "BeautyLine icon theme (V1 redesigned)";
    homepage    = "https://www.gnome-look.org/p/1425426/";
    platforms   = platforms.linux;
    license     = licenses.gpl3Only;
    maintainers = with maintainers; [ gvolpe ];
  };
}
