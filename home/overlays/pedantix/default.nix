self: super:

{
  pedantix = self.symlinkJoin {
    name = "pedantix-wrapped";
    nativeBuildInputs = [ self.makeWrapper ];
    paths = [ super.sources.pedantix.packages.${self.stdenv.hostPlatform.system}.pedantix ];
    postBuild = ''
      wrapProgram $out/bin/pedantix \
        --prefix PATH : ${self.lib.makeBinPath [ self.nixpkgs-fmt ]} \
        --add-flags "--formatter nixpkgs-fmt"
    '';
  };
}
