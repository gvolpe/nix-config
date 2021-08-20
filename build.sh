#! /usr/bin/env bash

# Shows the output of every command
set +x

# Manual steps
mkdir -p $HOME/.config/polybar/logs
touch $HOME/.config/polybar/logs/bottom.log
touch $HOME/.config/polybar/logs/top.log
mkdir -p $HOME/.cache/fzf-hoogle
touch $HOME/.cache/fzf-hoogle/cache.json

# Home manager dirs
mkdir -p $HOME/.config/nixpkgs/
cp -r home/* $HOME/.config/nixpkgs/
mkdir -p $HOME/.config/Pictures/
cp home/nixos.png $HOME/Pictures/

# Set user's profile picture for Gnome3
#sudo cp home/gvolpe.png /var/lib/AccountsService/icons/gvolpe
#sudo echo "Icon=/var/lib/AccountsService/icons/gvolpe" >> /var/lib/AccountsService/users/gvolpe

# Set screenlock wallpaper
#multilockscreen -u home/nixos.png
