nix-config
==========

[![CI Home](https://github.com/gvolpe/nix-config/workflows/Home/badge.svg)](https://github.com/gvolpe/nix-config/actions)
[![CI System](https://github.com/gvolpe/nix-config/workflows/NixOS/badge.svg)](https://github.com/gvolpe/nix-config/actions)

My current — and always evolving — NixOS configuration files, home-manager, neovim, etc.

![scala-dev-env](imgs/scala-dev.png)

![desktop](imgs/desktop-1.jpg)

![amd](imgs/amd.jpg)

![themes](imgs/theme.jpg)

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
│  ├── display
│  ├── home.nix
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
│  ├── installation.nix
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
- `outputs`: the Home Manager, installation shell and NixOS flake outputs.
- `system`: the NixOS configuration, settings for different laptops and window managers.

## Install

You can have a look at the available flake outputs before getting started.

```console
$ nix flake show github:gvolpe/nix-config
github:gvolpe/nix-config/60b91aea6461cfb8fb1efdc9fb88f8c5ba815810
├───devShell
│   └───x86_64-linux: development environment 'installation-shell'
├───homeConfigurations: unknown
└───nixosConfigurations
    ├───dell-xps: NixOS configuration
    └───tongfang-amd: NixOS configuration
```

### NixOS

The full home configuration is not yet fully automated but the NixOS configuration can be installed by running the following command.

```console
$ nixos-rebuild build --flake github:gvolpe/nix-config#tongfang-amd
```

Beware that the `hardware-configuration.nix` file is a dummy file, you'd probably want to use the one detected by NixOS, which should be placed under `/etc/nixos`.

### Home Manager

A fresh install requires the creation of certain directories so this has not been automated yet (see `build` script file). However, if you omit those steps, the entire HM configuration can also be built as any other flake.

```console
$ nix build github:gvolpe/nix-config#homeConfigurations.gvolpe-hdmi.activationPackage
$ result/activate
```

### Full configuration via script

On a fresh NixOS installation, run the following commands.

```shell
mkdir DELETE_ME && cd DELETE_ME
nix develop github:gvolpe/nix-config -c \
  "wget -c https://github.com/gvolpe/nix-config/archive/master.tar.gz && tar --strip-components=1 -xvf master.tar.gz"
chmod +x build && ./build
```
