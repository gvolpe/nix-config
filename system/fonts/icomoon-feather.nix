{ lib, fetchurl }:

let
  pname   = "icomoon-feather";
  version = "5c94ed951cf5342678bd9bd9a5a564e5b5c64822";
in 
  fetchurl {
    name = "${pname}-${version}";
    url = "https://raw.githubusercontent.com/adi1090x/polybar-themes/5c94ed951cf5342678bd9bd9a5a564e5b5c64822/polybar-5/fonts/${pname}.ttf";

    downloadToTemp = true;
    recursiveHash = true;

    postFetch = ''
      install -D $downloadedFile $out/share/fonts/truetype/${pname}.ttf
    '';
    
    sha256 = "0xypj93rk61dl2y3b4idyia83kq8i074l9v8v2mps21rxi82551c";
  }
