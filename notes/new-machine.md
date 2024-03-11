# Fresh install on a new machine

On a fresh NixOS installation, run the following commands.

```console
nix develop nixpkgs#git # optional if you don't have git, it's needed by the next command
nix flake clone github:gvolpe/nix-config --dest /choose/a/path
./build fresh-install # requires sudo
reboot
```

## SSH key

If you have a private repo depending on the `nix-config` (like I do), you'll need to create a new SSH key and add it to your Github settings.

```console
gen-ssh-key your@email.com
```

Then you'll copy the output of the following command and set it on your Github SSH settings.

```console
cat ~/.ssh/id_ed25519.pub
```

You should now be able to clone a private repo via SSH.

```console
git clone git@github.com:gvolpe/private-flake.git
```

NOTE: the `gen-ssh-key` should be installed by Home Manager (defined [here](../home/scripts/gen-ssh-key.nix)).

## GPG migration & secrets

Once `git-crypt` is installed (set up via Home Manager in the previous step), we'll need a valid GPG key to decode secrets. The GPG key can be imported from another machine as follows:

1. Export private GPG key on the existing machine.

```console
gpg --export-secret-keys 0x121D4302A64B2261 > private-key
```

2. Copy private GPG key to the new machine.

```console
$ scp USER@HOST:/path/to/private-key .
# for example
scp gvolpe@tongfang-amd:/home/gvolpe/workspace/private-key .
(gvolpe@tongfang-amd) Password:
```

3. Import private GPG key on the new machine.

```console
gpg --import private-key
```

4. Decode secrets on the private repo.

```console
git-crypt unlock
```
