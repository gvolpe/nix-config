self: super:

let
  newcode = ''
    if (!appName) appName = pa_proplist_gets(i->proplist, PA_PROP_MEDIA_NAME);
    if (appName) {
  '';
in
{
  sway-audio-idle-inhibit = super.sway-audio-idle-inhibit.overrideAttrs (old: {
    preConfigure = (old.preConfigure or "") + ''
      substituteInPlace src/pulse.cpp \
        --replace-fail "if (appName) {" "${newcode}"
    '';
  });
}
