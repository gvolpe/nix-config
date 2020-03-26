#!/bin/bash

# Shows the output of every command
set +x

# Install Nix
curl https://nixos.org/nix/install | sh

# Configure Nix
mkdir -p $HOME/.config/nixpkgs && cp config.nix $HOME/.config/nixpkgs/.

# Install software
nix-env -irf packages.nix

# Install Haskell IDE
cachix use ghcide-nix
nix-env -iA ghcide-ghc865 -f https://github.com/cachix/ghcide-nix/tarball/master

# Configure software
chmod +x scripts/config.sh
scripts/config.sh
