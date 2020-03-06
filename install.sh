#!/bin/bash

# Install Nix
curl https://nixos.org/nix/install | sh

# Configure Nix
mkdir -p ~/.config/nixpkgs && cp confix.nix ~/.config/nixpkgs/.

# Install software
nix-env -irf packages.nix

# Install Haskell IDE
cachix use ghcide-nix
nix-env -iA ghcide-ghc865 -f https://github.com/cachix/ghcide-nix/tarball/master

# Configure software
scripts/config.sh
