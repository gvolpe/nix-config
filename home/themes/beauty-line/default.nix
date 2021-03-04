{ lib, stdenv, fetchzip, breeze-icons, gtk3, gnome-icon-theme, hicolor-icon-theme, mint-x-icons, pantheon }:

stdenv.mkDerivation rec {
  pname   = "BeautyLine";
  version = "0.0.1";

  src = fetchzip {
    name   = "Custom-${pname}-${version}";
    url    = "https://github.com/gvolpe/BeautyLine/releases/download/${version}/CustomBeautyLine.tar.gz";
    sha256 = "15iyr3imixxqa0irlnh2jm54bvfby6la6d5ix03y8bj1yk1j3ypk";
  };

  nativeBuildInputs = [ gtk3 ];

  propagatedBuildInputs = [
    breeze-icons
    gnome-icon-theme
    hicolor-icon-theme
    mint-x-icons
    pantheon.elementary-icon-theme
  ];

  dontDropIconThemeCache = true;

  installPhase = ''
    mkdir -p $out/share/icons/${pname}
    cp -r * $out/share/icons/${pname}/
    gtk-update-icon-cache $out/share/icons/${pname}
  '';

  meta = with lib; {
    description = "BeautyLine icon theme (with customized status icons)";
    homepage    = "https://www.gnome-look.org/p/1425426/";
    platforms   = platforms.linux;
    maintainers = with maintainers; [ gvolpe ];
  };
}
