# NixOS on flakes

https://nixos.wiki/wiki/Flakes

## Upgrade nixpkgs

```console
$ nix flake update
$ nix build .#flake-name
```

Or in a single command.

```console
$ nix build .#flake-name --recreate-lock-file
```

## Update specific input flake 

```console 
$ nix flake lock --update-input neovim-flake
```

## List generations

```console
$ sudo nix-env --list-generations --profile /nix/var/nix/profiles/system
  86   2021-10-28 10:26:33
  87   2022-01-05 17:39:10
  88   2022-01-05 17:57:11
  89   2022-01-06 11:44:20
  90   2022-01-12 14:27:06
  91   2022-01-12 14:42:09
  92   2022-01-12 14:42:30   (current)
```

## Fresh install

Perhaps we also need to add the `--impure` flag.

```console
$ nixos-install --root /mnt --flake github:gvolpe/nix-config#tongfang-amd
```
