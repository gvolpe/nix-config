#! /usr/bin/env bash

# Shows the output of every command
set +x

# Pin Nixpkgs to NixOS unstable on June 15th of 2020
export PINNED_NIX_PKGS="https://github.com/NixOS/nixpkgs-channels/archive/0a146054bdf.tar.gz"

# Switch to the unstable channel
sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
sudo nixos-rebuild -I nixpkgs=$PINNED_NIX_PKGS switch --upgrade

# Nix configuration
sudo cp nixos/configuration.nix /etc/nixos/
cp -r nixos/home/* $HOME/.config/nixpkgs/

# Home manager - Pin to master on June 15th of 2020
nix-channel --add https://github.com/rycee/home-manager/archive/ad4f33c.tar.gz home-manager
nix-channel --update
nix-shell '<home-manager>' -A install
home-manager switch
