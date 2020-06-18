nix-config
==========

NixOS configuration files, home-manager, neovim, etc.

### Install

On a fresh NixOS installation, run the following command:

```
nix-shell --run "wget -c https://github.com/gvolpe/nix-config/archive/master.tar.gz && tar --strip-components=1 -xvf master.tar.gz" -p wget s-tar
./install.sh
```
