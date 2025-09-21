# X11 Scripts

A few useful scripts mainly for the XMonad setup (previously on `switch`).

```console
activate_hm() {
  BAKFILE=~/.config/orage/oragerc.bak
  if [ -f "$BAKFILE" ]; then
    rm $BAKFILE
  fi

  HOME_MANAGER_BACKUP_EXT=bak result/activate
}

switch_display() {
  autorandr --change
  systemctl --user restart polybar.service
}

restart_X() {
  echo "⚠️ Restarting X11 (requires authentication) ⚠️"
  systemctl restart display-manager
}
```

### Fresh Install

```console
# for fresh install only
pre_home() {
  echo "Creating config / cache directories..."

  # Polybar logs
  mkdir -p $HOME/.config/polybar/logs
  touch $HOME/.config/polybar/logs/bottom.log
  touch $HOME/.config/polybar/logs/top.log

  # FZF cache
  mkdir -p $HOME/.cache/fzf-hoogle
  touch $HOME/.cache/fzf-hoogle/cache.json

  # Desktop pic
  mkdir -p $HOME/Pictures/
  cp imgs/nixos.png $HOME/Pictures/

  # Nix daemon config for normal user
  mkdir -p $HOME/.config/nix
  cp home/daemon.conf $HOME/.config/nix/nix.conf
}

post_home() {
  # Set user's profile picture for Gnome3
  echo "Setting profile picture for the accounts service"
  sudo cp imgs/gvolpe.png /var/lib/AccountsService/icons/gvolpe
  sudo echo "Icon=/var/lib/AccountsService/icons/gvolpe" >> /var/lib/AccountsService/users/gvolpe

  # Set screenlock wallpaper
  echo "Setting screen-lock wallpaper"
  multilockscreen -u imgs/nixos.png
}
# end of fresh install section
```
