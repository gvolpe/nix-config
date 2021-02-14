{ pkgs, ...}:

let
  add    = "${pkgs.openssh}/bin/ssh-add";
  agent  = "${pkgs.openssh}/bin/ssh-agent";
  keygen = "${pkgs.openssh}/bin/ssh-keygen";
in
  pkgs.writeShellScriptBin "gen-ssh-key" ''
    ${keygen} -t ed25519 -C $1
    eval $(${agent} -s)
    ${add} $HOME/.ssh/id_rsa
  ''
