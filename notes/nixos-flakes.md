# NixOS on flakes

https://nixos.wiki/wiki/Flakes

## Upgrade nixpkgs

```shell
nix flake update
nix build .#flake-name
```

Or in a single command.

```shell
nix build .#flake-name --recreate-lock-file
```
