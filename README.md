nix-config
==========

![ci-badge](https://img.shields.io/static/v1?label=Built%20with&message=nix&color=blue&style=flat&logo=nixos&link=https://nixos.org&labelColor=111212)
[![built with garnix](https://img.shields.io/endpoint?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fgvolpe%2Fnix-config%3Fbranch%3Dmaster)](https://garnix.io)

My current — and always evolving — NixOS configuration files, home-manager, neovim, etc.

![neofetch](imgs/neofetch.png)

![cowsay](imgs/cowsay.png)

![scala-dev-env](imgs/scala-dev.png)

![desktop](imgs/desktop-1.jpg)

![themes](imgs/theme.jpg)

![demo](imgs/demo.png)

NOTE: My new neovim configuration is now a separate flake: https://github.com/gvolpe/neovim-flake

## Programs

The `home.nix` file contains details about all the software I use, but here's a shout-out to the ones I use the most and that are customized to my needs.

| Type           | Program      |
| :------------- | :----------: |
| Editor         | [NeoVim](https://neovim.io/) |
| Launcher       | [Rofi](https://github.com/davatorium/rofi) |
| Shell          | [Fish](https://fishshell.com/) |
| Status Bar     | [Polybar](https://polybar.github.io/) |
| Terminal       | [Alacritty](https://github.com/alacritty/alacritty) |
| Window Manager | [XMonad](https://xmonad.org/) |

## Themes

| Type           | Name      |
| :------------- | :----------: |
| GTK Theme      | [Juno Ocean](https://github.com/EliverLara/Juno) |
| GTK Icon Theme | [Beauty Line](https://www.gnome-look.org/p/1425426/) |
| Terminal Font  | [JetBrainsMono](https://www.jetbrains.com/lp/mono/) |

## Structure

Here is an overview of the directories' structure:

```
.
├── build
├── flake.nix
├── flake.lock
├── switch
├── home
├── imgs
├── lib
├── notes
├── outputs
└── system
```

- `build`: the build and installation script.
- `flake.nix`: home and system configurations.
- `switch`: helper script to switch home and system configurations.
- `home`: all the user programs, services and dotfiles.
- `imgs`: screenshots and other images.
- `lib`: custom nix library functions and overlays used to instantiate pkgs.
- `notes`: cheat-sheets, docs, etc.
- `outputs`: the Home Manager and NixOS flake outputs.
- `system`: the NixOS configuration, settings for different laptops and window managers.

## Install

You can have a look at the available flake outputs before getting started.

```console
$ nix flake show github:gvolpe/nix-config
github:gvolpe/nix-config/b7fb45d60b761fe39ee1ce78d2b1fd0f0c8db50e
├───homeConfigurations: unknown
├───nixosConfigurations
│   ├───dell-xps: NixOS configuration
│   ├───edp-tongfang-amd: NixOS configuration
│   └───tongfang-amd: NixOS configuration
└───packages
    └───x86_64-linux
        ├───bazecor: package 'bazecor-1.3.9-patched'
        ├───metals: package 'metals-1.2.0'
        └───metals-updater: package 'metals-updater-script'
```

As well as all the declared flake inputs.

```console
$ nix flake metadata github:gvolpe/nix-config
```

The `edp-tongfang-amd` configuration also contains my Home Manager configuration using the NixOS module, so it can easily be tested with a single command.

```console
$ nixos-rebuild switch --flake github:gvolpe/nix-config#edp-tongfang-amd
```

Or you can test it directly on a QEMU virtual machine, though it has its limitations in terms of graphics.

```console
$ nixos-rebuild build-vm --flake github:gvolpe/nix-config#edp-tongfang-amd
./result/bin/run-tongfang-amd-vm
```

Having both NixOS and Home Manager configurations combined makes it easier to quickly install it on a new machine, but my preference is to have both separate, as my Home Manager configuration changes more often than that of the NixOS one, resulting in multiple generations at boot time.

Managing the different Home Manager generations in isolation makes this way easier for me.

### NixOS

The NixOS configuration can be installed by running the following command.

```console
$ nixos-rebuild switch --flake github:gvolpe/nix-config#tongfang-amd
```

Beware that the `hardware-configuration.nix` file is the result of the hardware scan of the specific machine and might not be suitable for yours.

### Home Manager

A fresh install requires the creation of certain directories (see what the `build` script does). However, if you omit those steps, the entire HM configuration can also be built as any other flake.

```console
$ nix build github:gvolpe/nix-config#homeConfigurations.gvolpe-edp.activationPackage
$ result/activate
```

### Full configuration via script

On a fresh NixOS installation, run the following commands.

```console
$ nix flake clone github:gvolpe/nix-config --dest /choose/a/path
$ ./build fresh-install # requires sudo
```

There's an additional step required if you want to have secrets working.

```console
$ nix run nixpkgs#git-crypt unlock
```

> NOTE: it requires your GPG Keys to be correctly set up.

The `build` script is only suitable for a fresh install customized to my personal use but you can build the flakes directly. E.g.

```console
$ nix build .#nixosConfigurations.tongfang-amd.config.system.build.toplevel
sudo result/bin/switch-to-configuration switch
```

Or for Home Manager.

```console
$ nix build .#homeConfigurations.gvolpe-hdmi.activationPackage
$ result/activate
```
