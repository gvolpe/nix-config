{ lib, stdenv, gtk3, gnome-icon-theme, hicolor-icon-theme }:

stdenv.mkDerivation rec {
  pname   = "BeautyLine";
  version = "0.0.1";

  src = builtins.fetchTarball {
    name   = "${pname}-${version}";
    url    = "https://github.com/gvolpe/BeautyLine/releases/download/${version}/BeautyLine.tar.gz";
    sha256 = "0s9zpxh8s93awnkw30xnxmpnqsw6wx1fvyvjh33fs3r40vv7lxgg";
  };

  nativeBuildInputs = [ gtk3 ];

  propagatedBuildInputs = [ gnome-icon-theme hicolor-icon-theme ];

  dontDropIconThemeCache = true;

  installPhase = ''
    mkdir -p $out/share/icons/${pname}
    cp -r * $out/share/icons/${pname}/
    gtk-update-icon-cache $out/share/icons/${pname}
  '';

  meta = with lib; {
    description = "BeautyLine icon theme";
    homepage    = "https://www.gnome-look.org/p/1425426/";
    platforms   = platforms.linux;
    maintainers = with maintainers; [ gvolpe ];
  };
}
