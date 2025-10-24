self: super:

{
  wshowkeys-mao = super.wshowkeys.overrideAttrs (_: {
    pname = "wshowkeys-mao-git";
    version = super.sources.wshowkeys.rev;
    src = super.sources.wshowkeys;
  });
}
