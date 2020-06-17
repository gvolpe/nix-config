#! /usr/bin/env bash

# Shows the output of every command
set +x

# Switch to the unstable channel
sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
sudo nixos-rebuild switch --upgrade

# Nix configuration
sudo cp nixos/configuration.nix /etc/nixos/
cp -r nixos/home/* $HOME/.config/nixpkgs/

# Home manager
nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
nix-channel --update
nix-shell '<home-manager>' -A install
home-manager switch
