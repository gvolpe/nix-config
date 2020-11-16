### Distribute a binary

```sh
cabal new-configure --disable-executable-dynamic --ghc-option=-optl=-static --ghc-option=-optl=-pthread
```

If using `cabal2nix` then:

```sh
nix-build | cachix push <name>
```

The binary will be left at `./result/bin/`, no dependencies. Copy it, change the permissions and ship it!

```sh
cp ./result/bin/my-app ~/my-app && cd ~/
chown <user> my-app
tar -czvf my-app.tar.gz my-app
```
