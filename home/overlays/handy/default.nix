self: super:

{
  # add wayland text input tool to handy's path
  handy = self.symlinkJoin {
    name = "handy-wrapped";
    nativeBuildInputs = [ self.makeWrapper ];
    paths = [ super.handy ];
    postBuild = ''
      wrapProgram $out/bin/handy \
        --prefix PATH : ${self.lib.makeBinPath [ self.wtype ]}
    '';
  };
}
