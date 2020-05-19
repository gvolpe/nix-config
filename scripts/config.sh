#!/bin/bash

set +x

# Git config
cp git/* $HOME/.

# NeoVim config
curl -fLo $HOME/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
mkdir -p $HOME/.local/share/nvim/plugged
mkdir -p $HOME/.config/nvim
cp nvim/* $HOME/.config/nvim/.

# Install plugins and extensions
nvim -c 'PlugInstall|qa'
nvim -c 'CocInstall -sync coc-metals coc-yarn|qa'

# FZF config (preview with bat, search with ripgrep or fd)
echo '# FZF config' >> $HOME/.bashrc
echo $'export FZF_DEFAULT_OPTS="--preview=\'bat {} --color=always\'"' >> $HOME/.bashrc
echo 'export SKIM_DEFAULT_COMMAND="rg --files || fd || find ."' >> $HOME/.bashrc

# Fixing locales
echo 'export LC_ALL="en_US.UTF-8"' >> $HOME/.bashrc
echo 'export LOCALE_ARCHIVE_2_11="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"' >> $HOME/.bashrc
echo 'export LOCALE_ARCHIVE_2_27="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"' >> $HOME/.bashrc
echo 'export LOCALE_ARCHIVE="/usr/bin/locale"' >> $HOME/.bashrc

# Spotify in 4k display
echo 'alias spotify="$HOME/.nix-profile/bin/spotify --force-device-scale-factor=2.0"' >> $HOME/.bashrc

# Toggle touchpad
cp scripts/toggle-touchpad.sh $HOME/.

# Git parser
cat scripts/git-parser >> $HOME/.bashrc

# Overriding common tools like vi, cat and ls
echo 'alias vi=nvim' >> $HOME/.bashrc
echo 'alias vim=nvim' >> $HOME/.bashrc
echo 'alias cat=bat' >> $HOME/.bashrc
echo 'alias ls=exa' >> $HOME/.bashrc
echo 'alias ping=prettyping' >> $HOME/.bashrc
echo 'alias du="ncdu --color dark -rr -x"' >> $HOME/.bashrc

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

# Direnv hook
echo 'eval "$(direnv hook bash)"' >> $HOME/.bashrc

# Nix-direnv
mkdir -p $HOME/.config/direnv
cp nix-direnv/direnvrc $HOME/.config/direnv/.

# Hoogle database
hoogle generate

# Desktop applications with icons
sudo cp desktop/* /usr/share/applications/.

# Other packages are better installed using `aptitude` since Nix support is not great outside NixOS.
sudo apt update
sudo apt install calibre docker docker-compose gimp gnome-tweak-tool terminator vlc -y

# Gnome settings (keybindings, etc) - requires gnome-tweak-tool
dconf load / < gnome/settings.dconf

echo ">>> Installation & Configuration DONE <<<"
