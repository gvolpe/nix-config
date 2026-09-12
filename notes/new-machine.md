# Fresh install on a new machine

On a fresh NixOS installation, run the following commands.

```console
nix develop nixpkgs#git # optional if you don't have git, it's needed by the next command
nix flake clone github:gvolpe/nix-config --dest /choose/a/path
```

First install the Home Manager stuff (disable `home.changes-report.report` before doing so, otherwise it'll fail because the `nvd` package is not yet available), e.g.

```console
nix build .#homeConfigurations.niri-desktop.activationPackage
result/activate
```

Then install the desired NixOS host configuration and reboot.

```console
sudo nixos-rebuild switch --flake .#host
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

We'll need a valid GPG key to decode secrets. The GPG key can be imported from another machine as follows:

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

## Agenix identity

Agenix secrets need one matching private key at activation time. Do this before the first Home Manager activation that depends on `home/secrets`.

The fastest path is to copy the existing age identity from a working machine.

```console
mkdir -p ~/.config/agenix
chmod 700 ~/.config/agenix
scp USER@HOST:/home/gvolpe/.config/agenix/identity.txt ~/.config/agenix/identity.txt
chmod 600 ~/.config/agenix/identity.txt
```

Alternatively, create a new identity on the new machine and rekey the secrets from a machine that can already decrypt them.

```console
mkdir -p ~/.config/agenix
chmod 700 ~/.config/agenix
nix shell nixpkgs#age -c age-keygen -o ~/.config/agenix/identity.txt
chmod 600 ~/.config/agenix/identity.txt
nix shell nixpkgs#age -c age-keygen -y ~/.config/agenix/identity.txt
```

Add the printed public key to `home/secrets/secrets.nix`, then on a working machine run:

```console
cd home/secrets
agenix --rekey -i ~/.config/agenix/identity.txt
```

Commit and pull the rekeyed `.age` files on the new machine before activating Home Manager.
