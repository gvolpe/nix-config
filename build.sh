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
  nix-shell -p nix-build-uncached --run "nix-build-uncached -A home"
}

build_ci_system() {
  nix build .#nixosConfigurations.dell-xps.config.system.build.toplevel
  nix build .#nixosConfigurations.tongfang-md.config.system.build.toplevel
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
  sudo cp system/flake.* /etc/nixos/
  sudo cp system/configuration.nix /etc/nixos/
  sudo cp -r system/fonts/ /etc/nixos/
  sudo cp -r system/machine/ /etc/nixos/
  sudo cp -r system/wm/ /etc/nixos/
  sudo nixos-rebuild switch --flake '.#tongfang-amd'
}

build_all() {
  echo "No custom build option given. Building system and home from scratch."
  nix-shell -p cachix --command '
    cachix use gvolpe-nixos
    ./build.sh home
    ./build.sh system
  '
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
