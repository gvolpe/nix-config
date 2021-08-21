#! /usr/bin/env bash

# Shows the output of every command
set +x

build_home() {
  echo "Creating config / cache directories..."

  # Polybar logs
  mkdir -p $HOME/.config/polybar/logs
  touch $HOME/.config/polybar/logs/bottom.log
  touch $HOME/.config/polybar/logs/top.log

  # FZF cache
  mkdir -p $HOME/.cache/fzf-hoogle
  touch $HOME/.cache/fzf-hoogle/cache.json

  # Home manager files
  mkdir -p $HOME/.config/nixpkgs/
  cp -r home/* $HOME/.config/nixpkgs/

  # Desktop pic
  mkdir -p $HOME/Pictures/
  cp home/nixos.png $HOME/Pictures/

  # Install Home Manager
  echo "Installing Home Manager..."
  nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
  nix-channel --update
  export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
  nix-shell '<home-manager>' -A install

  # Run Home Manager
  echo "Running Home Manager build..."
  home-manager build
}

build_home_extra() {
  # Set user's profile picture for Gnome3
  sudo cp home/gvolpe.png /var/lib/AccountsService/icons/gvolpe
  sudo echo "Icon=/var/lib/AccountsService/icons/gvolpe" >> /var/lib/AccountsService/users/gvolpe

  # Set screenlock wallpaper
  multilockscreen -u home/nixos.png
}

build_system() {
  sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
  sudo cp system/configuration.nix /etc/nixos/
  sudo cp -r system/fonts/ /etc/nixos/
  sudo cp -r system/machine/ /etc/nixos/
  sudo cp -r system/wm/ /etc/nixos/
  sudo nixos-rebuild -I nixpkgs=$(cat PINNED_NIXPKGS) switch --upgrade
}

build_all() {
  echo "No custom build option given. Building system and home."
  build_system
  nix-shell -p cachix --run "cachix use gvolpe-nixos"
  build_home
  home-manager switch
  build_home_extra
}

case $1 in
  "home")
    build_home;;
  "home-extra")
    build_home_extra;;
  "system")
    build_system;;
  *)
    build_all;;
esac
