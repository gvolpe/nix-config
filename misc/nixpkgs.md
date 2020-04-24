# Nixpkgs

A few helpful commands to get started.

## What version am I running?

You can find out what channels you're subscribed to with the command below:

```nix
$ nix-channel --list
nixpkgs https://nixos.org/channels/nixpkgs-unstable
```

Now, in order to find out what version of that channel you're on, run the following command:

```nix
$ nix-instantiate --eval -E '(import <nixpkgs> {}).lib.version'
"20.09pre214374.1fe82110feb"
```

The last part of the name (after the `.`) is what you can use for the `fetchTarball` command to pin packages. For
example:

```nix
let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/1fe82110feb.tar.gz") {};
```
