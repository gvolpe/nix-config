self: super:

{
  wshowkeys-mao = super.wshowkeys.overrideAttrs (_: {
    pname = "wshowkeys-mao-git";
    version = "2025-06-24";

    src = super.fetchFromGitHub {
      owner = "DreamMaoMao";
      repo = "wshowkeys";
      rev = "bf18fba01eafd7659e242066abca0fd4938bf417";
      hash = "sha256-KTtrmICFXXYBcPcGfZ32/UjQAEHSXF79PvSLCKbuCPw=";
    };
  });
}
