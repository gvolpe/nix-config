nix-config
==========

[![CI Home](https://github.com/gvolpe/nix-config/workflows/Home/badge.svg)](https://github.com/gvolpe/nix-config/actions)
[![CI System](https://github.com/gvolpe/nix-config/workflows/NixOS/badge.svg)](https://github.com/gvolpe/nix-config/actions)

My current — and always evolving — NixOS configuration files, home-manager, neovim, etc.

![scala-dev-env](imgs/scala-dev.png)

![desktop](imgs/desktop-1.jpg)

![amd](imgs/amd.jpg)

![themes](imgs/theme.jpg)

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

If you're interested in using `taffybar` or `xmobar`, browse the commit history and you will find the configuration files I used. Here's a [screenshot](imgs/taffybar.png) showcasing the former.

## Themes

| Type           | Name      |
| :------------- | :----------: |
| GTK Theme      | [Juno Ocean](https://github.com/EliverLara/Juno) |
| GTK Icon Theme | [Beauty Line](https://www.gnome-look.org/p/1425426/) |
| Terminal Font  | [JetBrainsMono](https://www.jetbrains.com/lp/mono/) |

## Structure

Here is an overview of the folders' structure:

```
.
├── build
├── flake.nix
├── flake.lock
├── home
│  ├── config.nix
│  ├── home.nix
│  ├── modules
│  ├── overlays
│  ├── programs
│  ├── scripts
│  ├── secrets
│  ├── services
│  └── themes
├── imgs
├── notes
├── outputs
│  ├── home-conf.nix
│  └── nixos-conf.nix
└── system
   ├── cachix
   ├── cachix.nix
   ├── configuration.nix
   ├── fonts
   ├── machine
   ├── misc
   └── wm
```

- `build`: the build and installation script.
- `flake.nix`: home and system configurations.
- `home`: all the user programs, services and dotfiles.
- `imgs`: screenshots and other images.
- `notes`: cheat-sheets, docs, etc.
- `outputs`: the Home Manager and NixOS flake outputs.
- `system`: the NixOS configuration, settings for different laptops and window managers.

## Install

You can have a look at the available flake outputs before getting started.

```console
$ nix flake show github:gvolpe/nix-config
github:gvolpe/nix-config/0161ea3bd15e0cd06696f27bd60c588991305b20
├───homeConfigurations: unknown
└───nixosConfigurations
    ├───dell-xps: NixOS configuration
    └───tongfang-amd: NixOS configuration
```

As well as all the declared flake inputs.

```console
$ nix flake metadata github:gvolpe/nix-config
```

### NixOS

The full home configuration is not yet fully automated but the NixOS configuration can be installed by running the following command.

```console
$ nixos-rebuild switch --flake github:gvolpe/nix-config#tongfang-amd
```

Beware that the `hardware-configuration.nix` file is the result of the hardware scan of the specific machine and might not be suitable for yours.

### Home Manager

A fresh install requires the creation of certain directories so this has not been automated yet (see `build` script file). However, if you omit those steps, the entire HM configuration can also be built as any other flake.

```console
$ nix build github:gvolpe/nix-config#homeConfigurations.gvolpe-hdmi.activationPackage
$ result/activate
```

### Full configuration via script

On a fresh NixOS installation, run the following commands.

```console
$ nix flake clone github:gvolpe/nix-config --dest /choose/a/path
$ nix run nixpkgs#git-crypt unlock
$ ./build fresh-install # requires sudo
```

> Note that `git-crypt unlock` requires your GPG Keys to be correctly set up.

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
