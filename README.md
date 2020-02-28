nix-config
==========

Nix configuration for the software I use.

## How to install

1. Install Nix

```
> curl https://nixos.org/nix/install | sh
```

More info: https://nixos.org/nix/download.html

2. Install packages

```
> nix-env -irf packages.nix
```

3. Run configuration commands

```
> ./init.sh
```

## Native packages

Other packages are better installed using `aptitude` since Nix support is not great outside NixOS.

```
sudo apt install calibre docker docker-compose terminator
```
