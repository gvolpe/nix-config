#!/bin/bash

set +x

# Git config
git config --global user.email "volpegabriel@gmail.com"
git config --global user.name "Gabriel Volpe"

# Git aliases
git config --global alias.amend 'commit --amend -m'
git config --global alias.br branch
git config --global alias.ca 'commit -am'
git config --global alias.cm 'commit -m'
git config --global alias.co checkout
git config --global alias.dc 'diff --cached'
git config --global alias.ls 'log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate'
git config --global alias.ll 'log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat'
git config --global alias.st status

# NeoVim config
curl -fLo $HOME/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
mkdir -p $HOME/.local/share/nvim/plugged
mkdir -p $HOME/.config/nvim
cp nvim/* $HOME/.config/nvim/.

# Fixing locales
echo 'export LC_ALL="en_US.UTF-8"' >> $HOME/.bashrc
echo 'export LOCALE_ARCHIVE_2_11="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"' >> $HOME/.bashrc
echo 'export LOCALE_ARCHIVE_2_27="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"' >> $HOME/.bashrc
echo 'export LOCALE_ARCHIVE="/usr/bin/locale"' >> $HOME/.bashrc

# Spotify in 4k display
echo ">>> Configuring Spotify in 4k"
echo 'alias spotify="$HOME/.nix-profile/bin/spotify --force-device-scale-factor=2.0"' >> $HOME/.bashrc
echo ">>> Remember to configure media keys for Spotify: https://fabianlee.org/2016/05/25/ubuntu-enabling-media-keys-for-spotify/"

# Toggle touchpad
cp scripts/toggle-touchpad.sh $HOME/.
echo ">>> Remember to add key-shortcut to Toggle Touchpad"

# Desktop applications with icons
echo ">>> Configuring desktop apps"
sudo cp desktop/* /usr/share/applications/.

# Git parser
cat scripts/git-parser >> $HOME/.bashrc

# SBT / Sonatype credentials
mkdir -p $HOME/.sbt/1.0
cp sbt/sonatype_credentials $HOME~/.sbt/.
cp sbt/sonatype.sbt $HOME/.sbt/1.0/.
echo ">>> Remember to set up username and password for Sonatype"

# SSH key
echo ">>> Configuring SSH key"
ssh-keygen -t rsa -b 4096 -C "volpegabriel@gmail.com"
eval "$(ssh-agent -s)"
ssh-add $HOME/.ssh/id_rsa

# Hoogle database
hoogle generate

echo ">>> Installation & Configuration DONE"
