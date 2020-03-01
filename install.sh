#!/bin/bash

# Install Nix
curl https://nixos.org/nix/install | sh

# Configure Nix
mkdir -p ~/.config/nixpkgs && cp confix.nix ~/.config/nixpkgs/.

# Install software
nix-env -irf packages.nix

# Configure software
scripts/config.sh
