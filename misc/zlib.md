## Zlib issues

Install GHC with package zlib:

```
nix-env -e ghc
nix-env -i -f '<nixpkgs>' -E 'pkgs: (pkgs {}).ghc.withPackages (hp: with hp; [ zlib ])'
```

Create symlinks to header files (search with `nix-locate`):

```
sudo ln -s /nix/store/j3anl3dw9f22g711g0y1bdn3hphgd4np-zlib-1.2.11-dev/include/zlib.h ~/.nix-profile/include/zlib.h

sudo ln -s /nix/store/j3anl3dw9f22g711g0y1bdn3hphgd4np-zlib-1.2.11-dev/include/zconf.h ~/.nix-profile/include/zconf.h
```

Export include and lib directories:

```
export CPATH=~/.nix-profile/include
export LIBRARY_PATH=~/.nix-profile/lib
```

### Related issues

- https://github.com/haskell/cabal/issues/6228
- https://github.com/NixOS/nixpkgs/issues/44144
