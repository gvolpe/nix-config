#! /usr/bin/env bash

# Shows the output of every command
set +x

prepare_home() {
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
}

install_hm() {
  echo "Installing Home Manager..."
  nix-channel --add $(cat ./pinned/home-manager) home-manager
  nix-channel --update
  export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
  nix-shell '<home-manager>' -A install
}

build_ci_home() {
  prepare_home
  nix-shell -p nix-build-uncached --run nix-build-uncached
}

build_ci_system() {
  cmd="
    nix-build-uncached '<nixpkgs/nixos>' \
      -I nixos-config=system/configuration.nix \
      -A system \
      --keep-going \
      --no-out-link
  "
  nix-shell -p nix-build-uncached --run "$cmd"
}

build_home() {
  prepare_home
  install_hm

  # Switch to HM's latest build
  echo "Running Home Manager switch..."
  home-manager switch

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
  sudo nixos-rebuild -I nixpkgs=$(cat ./pinned/nixpkgs) switch --upgrade
}

build_all() {
  echo "No custom build option given. Building system and home."
  build_system
  nix-shell -p cachix --run "cachix use gvolpe-nixos"
  build_home
}

case $1 in
  "ci-home")
    build_ci_home;;
  "ci-system")
    build_ci_system;;
  "home")
    build_home;;
  "system")
    build_system;;
  *)
    build_all;;
esac
