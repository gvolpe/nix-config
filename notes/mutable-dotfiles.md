# mutable dotfile 

Copy file under `XDG_CONFIG_HOME` that needs to be writable by X application. 

```nix 
{
  home.activation.writableFile = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [[ ! -e $XDG_CONFIG_HOME/secrets/writable-file ]]; then
      $DRY_RUN_CMD install $VERBOSE_ARG -Dm644 ${../file-to-copy} "$XDG_CONFIG_HOME/secrets/writable-file"
    fi
  '';
}
```
