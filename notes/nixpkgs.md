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

When the package is not a `tar.gz` you don't need to use `--unpack`. E.g.:

```
$ nix-prefetch-url https://github.com/lihaoyi/mill/releases/download/0.7.3/0.7.3
[0.0 MiB DL]
path is '/nix/store/f72vwx1dxcq5ia5qmq6ywskwvin7lbrs-0.7.3'
10rjhkncxswbh30jsq7j4is5ngp1r737j45xdwl9z1frmwz6l67d
```

## Start a Nix shell with a specific package version

Say we want to use `nodejs`. If we query for the existing packages this is what we get:

```
$ nix-env -qa nodejs
nodejs-10.20.1
nodejs-10.20.1
nodejs-12.16.3
nodejs-13.14.0
nodejs-14.3.0
```

Now how do we specify the version we want if we want to start a shell?

```
$ nix-shell -p nodejs-14.3.0
error: undefined variable 'nodejs-14' at (string):1:94
(use '--show-trace' to show detailed location information)
```

Oops! This doesn't work for a shell. There are a few ways we can retrieve the right name for the version we want. My favorite is `nix search`. E.g.:

```
$ nix search nodejs | rg 14
* nixpkgs.nodejs-14_x (nodejs)
* nixpkgs.nodejs-slim-14_x (nodejs-slim)
```

So we can now proceed with the desired version.

```
$ nix-shell -p nodejs-14_x
```

Note that you can also search for packages directly in your browser at https://nixos.org/nixos/packages.html?channel=nixpkgs-unstable.

## List package dependencies

For example, all the dependencies of `neovim`:

```
$ nix-store --query --references  $(nix-instantiate '<nixpkgs>' -A neovim)
```
