#!/bin/bash

# Shows the output of every command
set +x

# Install Nix
curl https://nixos.org/nix/install | sh
. $HOME/.nix-profile/etc/profile.d/nix.sh

# Configure Nix
mkdir -p $HOME/.config/nixpkgs && cp nix/config.nix $HOME/.config/nixpkgs/.

# Install software
nix-env -irf nix/packages.nix

# Configure software
chmod +x scripts/config.sh
scripts/config.sh
