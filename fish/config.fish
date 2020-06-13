# direnv hook
eval (direnv hook fish)

# Spotify in 4k display
alias spotify4k="~/.nix-profile/bin/spotify --force-device-scale-factor=2.0"

# TODO: See if this is still necessary
#set -x LC_ALL "en_US.UTF-8"
#set -x LOCALE_ARCHIVE_2_11 "$eval(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"
#set -x LOCALE_ARCHIVE_2_27 "$eval(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"
#set -x LOCALE_ARCHIVE "/usr/bin/locale"

alias tm="$HOME/tmux-sessions.sh"
alias ta="tmux a"
alias cat=bat
alias ls=exa

alias vim=nvim
alias vi=nvim

# FZF source
set PATH /home/gvolpe/.fzf/bin $PATH

# FZF key bindings
source /home/gvolpe/.fzf/shell/key-bindings.fish

# FZF config
set -x FZF_DEFAULT_OPTS "--preview='bat {} --color=always'"
set -x SKIM_DEFAULT_COMMAND "rg --files || fd || find ."
alias du "ncdu --color dark -rr -x"

set PATH /home/gvolpe/Downloads/dhall-lsp-server-1.0.7-x86_64-linux/bin $PATH
