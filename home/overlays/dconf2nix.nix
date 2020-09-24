self: super:

rec {
  dconf2nix = super.dconf2nix.overrideAttrs (
    old: rec {
      version = "v0.0.6";

      src = builtins.fetchurl {
        url    = "https://github.com/gvolpe/dconf2nix/releases/download/${version}/dconf2nix-linux-x86-64";
        sha256 = "1bh78hfgy4wnfdq184ck5yw72szllzl5sm7a3a4y46byq0xxklcd";
      };

      phases = ["installPhase" "patchPhase"];

      installPhase = ''
        mkdir -p $out/bin
        cp $src $out/bin/dconf2nix
        chmod +x $out/bin/dconf2nix
      '';
    }
  );
}
