self: super:

{
  wshowkeys-mao = super.wshowkeys.overrideAttrs (_: {
    pname = "wshowkeys-mao-git";
    version = super.xargs.wshowkeys-src.rev;
    src = super.xargs.wshowkeys-src;
  });
}
