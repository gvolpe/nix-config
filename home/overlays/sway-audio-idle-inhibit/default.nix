final: prev:

{
  sway-audio-idle-inhibit = prev.sway-audio-idle-inhibit.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      ./no-abort-on-logind-race.patch
      # https://github.com/ErikReider/SwayAudioIdleInhibit/pull/27
      ./ignore-source-output-improvements.patch
    ];
  });
}
