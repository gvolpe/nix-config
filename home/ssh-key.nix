{ pkgs, ...}:

let
  add    = "/run/current-system/sw/bin/ssh-add";
  keygen = "/run/current-system/sw/bin/ssh-keygen";
in
  pkgs.writeShellScriptBin "ssh-key" ''
    # Create SSH key
    ${keygen} -t rsa -b 4096 -C "youremail@domain.com"
    eval $(${ssh-agent} -s)
    ${add} $HOME/.ssh/id_rsa
  ''
