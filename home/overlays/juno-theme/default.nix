self: super:

{
  juno-theme = super.juno-theme.overrideAttrs (old: rec {
    version = "0.0.2";

    srcs = [
      (super.fetchurl {
        url = "https://github.com/gvolpe/Juno/releases/download/${version}/Juno.tar.xz";
        hash = "sha256-vRTTS77AjSJ3hzzxLRJ0JjWQOT+B9NLTI2b9QXmSpQc=";
      })
      (super.fetchurl {
        url = "https://github.com/gvolpe/Juno/releases/download/${version}/Juno-mirage.tar.xz";
        hash = "sha256-0oDxMH+/8MbDwO/3FSKxY189Kse81zkUDjkK4vVdsbY=";
      })
      (super.fetchurl {
        url = "https://github.com/gvolpe/Juno/releases/download/${version}/Juno-ocean.tar.xz";
        hash = "sha256-I+jfI5N9QRLcP2xNvEgzd/B+bBeruj1hXYj69buR/yM=";
      })
      (super.fetchurl {
        url = "https://github.com/gvolpe/Juno/releases/download/${version}/Juno-palenight.tar.xz";
        hash = "sha256-E6z4nGR/B9HdPVodyAYxKoFe/L+rtS5JbomzaGDcHck=";
      })
    ];
  });
}
