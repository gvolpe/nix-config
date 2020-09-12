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

Where `amd-config.nix` is defined as follows:

```nix
{pkgs,...} :

{
  boot.kernelPackages = pkgs.linuxPackages_5_8;

  boot.initrd.kernelModules = [ "amdgpu" ];
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "amdgpu" ];
}
```
