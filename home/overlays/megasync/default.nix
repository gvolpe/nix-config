self: super:

{
  megasync-scaled = super.symlinkJoin {
    name = "megasync";
    paths = [ super.megasync ];
    buildInputs = [ super.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/megasync --prefix QT_SCALE_FACTOR : 1
    '';
  };
}
