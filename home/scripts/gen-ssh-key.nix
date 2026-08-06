{ openssh, writeShellScriptBin }:

let
  add = "${openssh}/bin/ssh-add";
  agent = "${openssh}/bin/ssh-agent";
  keygen = "${openssh}/bin/ssh-keygen";
in
writeShellScriptBin "gen-ssh-key" ''
  ${keygen} -t ed25519 -C $1
  eval $(${agent} -s)
  ${add} $HOME/.ssh/id_ed25519
''
