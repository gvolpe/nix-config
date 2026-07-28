final: prev:

{
  wshowkeys-mao = prev.wshowkeys.overrideAttrs (_: {
    pname = "wshowkeys-mao-git";
    version = prev.sources.wshowkeys.rev;
    src = prev.sources.wshowkeys;
  });
}
