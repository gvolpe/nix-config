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
## Installing package from specific version

For example, Spotify:

```nix
nix-env -f https://github.com/NixOS/nixpkgs-channels/archive/nixos-20.03.tar.gz -iA spotify
```

## Getting SHA-256

```
nix-prefetch-url --unpack <url>
```

Example:

```
$ nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs-channels/archive/1fe82110feb.tar.gz
unpacking...
[17.6 MiB DL]
path is '/nix/store/6f8sd6qp6fqhw0pq5qli5izrqkhd7wv1-1fe82110feb.tar.gz'
08x6saa7iljyq2m0j6p9phy0v17r3p8l7vklv7y7gvhdc7a85ppi
```
