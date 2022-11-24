self: super:

{
  xdg-utils = super.xdg-utils.overrideAttrs (old: {
    postInstall = ''
      cp ${super.mimeo}/bin/mimeo $out/bin/xdg-open
    '' + old.postInstall;
  });
}
