{ lib, gram-ext, sources }:

gram-ext.buildGramRustExtension (attrs: {
  id = "scala";
  version = "0.2.4";

  src = sources.metals-zed;

  # the upstream lock file is git-ignored: https://github.com/scalameta/metals-zed/blob/a5228e7069bf9d4bb70bc42824e13dd41c161b47/.gitignore#L8
  cargoLock.lockFile = ./metals-zed-cargo.lock;

  prePatch = ''
    cp ${./metals-zed-cargo.lock} Cargo.lock
  '';

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
