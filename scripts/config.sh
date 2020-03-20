#!/bin/bash

# Git config
echo ">>> Configuring Git"
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
echo ">>> Configuring NeoVim"
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
mkdir ~/.local/share/nvim/plugged
cp nvim/* ~/.config/nvim/.

# Fixing locales
echo ">>> Configuring locales"
echo 'export LC_ALL="en_US.UTF-8"' >> ~/.bashrc
echo 'export LOCALE_ARCHIVE_2_11="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"' >> ~/.bashrc
echo 'export LOCALE_ARCHIVE_2_27="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"' >> ~/.bashrc
echo 'export LOCALE_ARCHIVE="/usr/bin/locale"' >> ~/.bashrc

# Spotify in 4k display
echo ">>> Configuring Spotify in 4k"
echo 'alias spotify="~/.nix-profile/bin/spotify --force-device-scale-factor=2.0"' >> ~/.bashrc
echo ">>> Remember to configure media keys for Spotify: https://fabianlee.org/2016/05/25/ubuntu-enabling-media-keys-for-spotify/"

# Toggle touchpad
cp toggle-touchpad.sh ~/.
echo ">>> Remember to add key-shortcut to Toggle Touchpad"

# Desktop applications with icons
echo ">>> Configuring desktop apps"
cp desktop/* ~/Desktop/.
chmod +x ~/Desktop/*
sudo cp desktop/* /usr/share/applications/.

# Git parser
cat scripts/git-parser >> ~/.bashrc

# SBT / Sonatype credentials
echo ">>> Configuring sbt-sonatype"
cp sbt/sonatype_credentials ~/.sbt/.
cp sbt/sonatype.sbt ~/.sbt/1.0/.
echo ">>> Remember to set up username and password for Sonatype"

# Hoogle database
hoogle generate

# SSH key
echo ">>> Configuring SSH key"
ssh-keygen -t rsa -b 4096 -C "volpegabriel@gmail.com"
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa

echo ">>> Installation & Configuration DONE"
