## Upgrade NixOS

```shell
sudo su
nix-channel --update nixos
nixos-rebuild switch
```

## Upgrade Nixpkgs

```shell
nix-env -u
home-manager switch
```
