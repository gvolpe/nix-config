nix-config
==========

Nix configuration of the packages I use.

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

- `i` -> install
- `r` -> remove all previous packages
- `f` -> force

