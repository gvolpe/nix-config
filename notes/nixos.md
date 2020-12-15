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

## Generate install ISO

We need Linux kernel v5.6x or greater in order to boot with AMD.

```shell
nixos-generate --list
nixos-generate -f install-iso -c amd-config.nix
```

See the custom `amd-config.nix` file, which contains `gparted` and uses Gnome3 by default

## Burn ISO to USB key

As documented at https://nixos.wiki/wiki/NixOS_Installation_Guide, find the USB device:

```shell
$ lsblk
NAME        MAJ:MIN RM   SIZE RO TYPE MOUNTPOINT
sda           8:0    1  28.8G  0 disk
└─sda1        8:1    1  28.8G  0 part
nvme0n1     259:0    0 465.8G  0 disk
├─nvme0n1p1 259:1    0 341.9G  0 part /
├─nvme0n1p2 259:2    0  22.8G  0 part
└─nvme0n1p3 259:3    0 101.1G  0 part /data
```

Note: the USB key must be unmounted. Now proceed to write the ISO into the right device (use the whole `/dev/sda`, not a partition like `/dev/sda1`).

```shell
sudo cp /nix/store/path_to_generated_iso /dev/sda
```

Example:

```shell
sudo cp /nix/store/6xj9zl5pa8308jc5i7fzf97789acchbl-nixos-20.09pre242769.61525137fd1-x86_64-linux.isonixos.iso/iso/nixos-20.09pre242769.61525137fd1-x86_64-linux.isonixos.iso /dev/sda
```

## AMD GPU

https://nixos.wiki/wiki/AMD_GPU
