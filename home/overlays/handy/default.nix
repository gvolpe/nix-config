final: prev:

{
  # add wayland text input tool to handy's path
  handy = prev.symlinkJoin {
    name = "handy-wrapped";
    nativeBuildInputs = [ prev.makeWrapper ];
    paths = [ prev.handy ];
    postBuild = ''
      wrapProgram $out/bin/handy \
        --prefix PATH : ${prev.lib.makeBinPath [ prev.wtype ]}
    '';
  };
}
