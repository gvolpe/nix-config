{ lib, stdenv, gtk3, gnome-icon-theme, hicolor-icon-theme }:

stdenv.mkDerivation rec {
  pname   = "BeautyLine";
  version = "0.0.1";

  src = builtins.fetchTarball {
    name   = "Customized-${pname}-${version}";
    url    = "https://github.com/gvolpe/BeautyLine/releases/download/${version}/CustomBeautyLine.tar.gz";
    sha256 = "0hkshs09j11n5ycfv6i91zhnfzy1l84wr2yyg9md1m46d8h1pbzp";
  };

  nativeBuildInputs = [ gtk3 ];

  propagatedBuildInputs = [ gnome-icon-theme hicolor-icon-theme ];

  dontDropIconThemeCache = true;

  installPhase = ''
    mkdir -p $out/share/icons/${pname}
    cp -r CustomBeautyLine/* $out/share/icons/${pname}/
    gtk-update-icon-cache $out/share/icons/${pname} || true
  '';

  meta = with lib; {
    description = "Customized BeautyLine icon theme with status icons";
    homepage    = "https://www.gnome-look.org/p/1425426/";
    platforms   = platforms.linux;
    maintainers = with maintainers; [ gvolpe ];
  };
}
