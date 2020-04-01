#!/bin/bash

set +x

# Git config
git config --global user.email "volpegabriel@gmail.com"
git config --global user.name "Gabriel Volpe"

# Global git ignore
touch $HOME/.gitignore_global
git config --global core.excludesfile '$HOME/.gitignore_global'
echo '*.bloop' >> $HOME/.gitignore_global
echo '*.metals' >> $HOME/.gitignore_global

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
echo 'alias spotify="$HOME/.nix-profile/bin/spotify --force-device-scale-factor=2.0"' >> $HOME/.bashrc

# Toggle touchpad
cp scripts/toggle-touchpad.sh $HOME/.

# Desktop applications with icons
sudo cp desktop/* /usr/share/applications/.

# Git parser
cat scripts/git-parser >> $HOME/.bashrc

# Overriding common tools like vi, cat and ls
echo 'alias vi=nvim' >> $HOME/.bashrc
echo 'alias vim=nvim' >> $HOME/.bashrc
echo 'alias cat=bat' >> $HOME/.bashrc
echo 'alias ls=exa' >> $HOME/.bashrc

# SBT / Sonatype credentials
mkdir -p $HOME/.sbt/1.0
cp sbt/sonatype_credentials $HOME~/.sbt/.
cp sbt/sonatype.sbt $HOME/.sbt/1.0/.
echo ">>> Remember to set up username and password for Sonatype"

# SSH key
ssh-keygen -t rsa -b 4096 -C "volpegabriel@gmail.com"
eval "$(ssh-agent -s)"
ssh-add $HOME/.ssh/id_rsa

# Tmux configuration (including patched font for Ubuntu)
mkdir -p $HOME/.local/share/fonts
cp tmux/Ubuntu-Mono-Nerd-Font-Complete.ttf $HOME/.local/share/fonts/.
cp tmux/tmux.conf $HOME/.tmux.conf
cp tmux/sessions.sh $HOME/tmux-sessions.sh
chmod +x $HOME/tmux-sessions.sh
echo 'alias tm="$HOME/tmux-sessions.sh"' >> $HOME/.bashrc
echo 'alias ta="tmux a"' >> $HOME/.bashrc
git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm

# Terminator configuration
mkdir -p $HOME/.config/terminator
cp terminator/config $HOME/.config/terminator/.

# Bloop bash autocompletion (not installed with Nix)
cp bash/bloop.bash $HOME/.bloop.bash
echo '[ -f ~/.bloop.bash ] && source ~/.bloop.bash' >> $HOME/.bashrc

# Hoogle database
hoogle generate

# Other packages are better installed using `aptitude` since Nix support is not great outside NixOS.
sudo apt install calibre docker docker-compose gimp gnome-tweak-tool terminator vlc -y

# Gnome settings (keybindings, etc) - requires gnome-tweak-tool
dconf load / < gnome/settings.dconf

echo ">>> Installation & Configuration DONE"
