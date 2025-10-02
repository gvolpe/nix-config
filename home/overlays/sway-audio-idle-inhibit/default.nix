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
      # https://github.com/ErikReider/SwayAudioIdleInhibit/issues/36
      substituteInPlace src/main.cpp \
        --replace-fail 'strtok_r(argv[++i], " ", &saveptr);' 'strtok_r(argv[++i], ",", &saveptr);'
      substituteInPlace src/main.cpp \
        --replace-fail 'token = strtok_r(nullptr, " ", &saveptr);' 'token = strtok_r(nullptr, ",", &saveptr);'

      # https://github.com/ErikReider/SwayAudioIdleInhibit/pull/27
      substituteInPlace src/pulse.cpp \
        --replace-fail "if (appName) {" "${newcode}"
    '';
  });
}
