final: prev:

{
  pedantix = prev.symlinkJoin {
    name = "pedantix-wrapped";
    nativeBuildInputs = [ prev.makeWrapper ];
    paths = [ prev.sources.pedantix.packages.${prev.stdenv.hostPlatform.system}.pedantix ];
    postBuild = ''
      wrapProgram $out/bin/pedantix \
        --prefix PATH : ${prev.lib.makeBinPath [ prev.nixpkgs-fmt ]} \
        --add-flags "--formatter nixpkgs-fmt"
    '';
  };
}
