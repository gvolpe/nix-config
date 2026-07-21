{ lib, gram-ext, sources }:

gram-ext.buildGramRustExtension (attrs: {
  id = "scala";
  version = "0.2.5-rc0";

  src = sources.metals-zed;

  cargoHash = "sha256-sdzJD7oczNtT4cBEB4n9md6Ob6xkdmj+vIABbxClJ7s=";

  # fix incompatibility with gram
  postPatch = ''
    if [ -f languages/scala/indents.scm ]; then
      rm languages/scala/indents.scm
    fi
  '';

  grammars.scala = sources.tree-sitter-scala;

  meta = {
    homepage = "https://github.com/scalameta/metals-zed";
    description = "Gram/Zed plugin for Metals (Scala)";
    license = lib.licenses.asl20;
  };
})
