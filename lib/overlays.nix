{ inputs, system }:

with inputs;

let
  cowsayOverlay = f: p: {
    inherit (cowsay.packages.${system}) cowsay;
  };

  fishOverlay = f: p: {
    inherit fish-bobthefish-theme fish-keytool-completions;
  };

  nixSearchOverlay = f: p: {
    nix-search = nix-search.packages.${system}.default;
  };

  metalsOverlay = f: p: {
    metals = p.callPackage ../home/programs/neovim-ide/metals.nix { };
    metals-updater = p.callPackage ../home/programs/neovim-ide/update-metals.nix { };
  };

  # nixos-version needs this to work with flakes
  libVersionOverlay = import "${inputs.nixpkgs}/lib/flake-version-info.nix" inputs.nixpkgs;

  libOverlay = f: p: rec {
    libx = import ./. { inherit (p) lib; };
    lib = (p.lib.extend (_: _: {
      inherit (libx) exe removeNewline secretManager;
    })).extend libVersionOverlay;
  };

  buildersOverlay = f: p: {
    mkHomeConfigurations = { pkgs ? f, extraImports ? { } }:
      import ../outputs/hm.nix { inherit extraImports inputs pkgs system; };

    mkNixosConfigurations = { pkgs ? f, extraSystemConfig ? { } }:
      import ../outputs/os.nix { inherit extraSystemConfig inputs pkgs system; };
  };

  treesitterGrammarsOverlay = f: p: {
    treesitterGrammars = _.withPlugins (p: [
      p.tree-sitter-scala
      p.tree-sitter-c
      p.tree-sitter-nix
      p.tree-sitter-elm
      p.tree-sitter-haskell
      p.tree-sitter-python
      p.tree-sitter-rust
      p.tree-sitter-markdown
      p.tree-sitter-markdown-inline
      p.tree-sitter-comment
      p.tree-sitter-toml
      p.tree-sitter-make
      # FIXME: typescript is broken at the minute
      #p.tree-sitter-tsx
      #p.tree-sitter-typescript
      #p.tree-sitter-html
      #p.tree-sitter-javascript
      #p.tree-sitter-css
      p.tree-sitter-graphql
      p.tree-sitter-json
      p.tree-sitter-smithy
    ]);
  };

  megasyncOverlay = f: p: {
    inherit (import inputs.nixpkgs-mega {
      inherit system;
      config.allowUnfree = true;
    }) megasync;
  };

  xargsOverlay = f: p: {
    xargs = {
      inherit (inputs) gh-md-toc penguin-fox;
      inherit (inputs.rycee-nurpkgs.lib.${system}) buildFirefoxXpiAddon;
      addons = f.nur.repos.rycee.firefox-addons;
    };
  };

  schemaOverlay = f: p: {
    nix-schema = inputs.nix-schema.packages.${system}.nix.overrideAttrs (old: {
      doCheck = false;
      doInstallCheck = false;
      postInstall = old.postInstall + ''
        rm $out/bin/nix-*
        mv $out/bin/nix $out/bin/nix-schema
      '';
    });
  };

  nixIndexDatabaseOverlay = f: p: {
    inherit (nix-index-database.packages.${system}) nix-index-database nix-index-small-database;
  };

  # pipewire overlay for broken zoom-us
  pipewireOverlay = f: p: {
    pipewire-zoom = inputs.nixpkgs-zoom.legacyPackages.${system}.pipewire;
  };

  # globalprotect vpn overlay for no-longer supported package
  globalProtectOverlay = f: p: {
    inherit (inputs.nixpkgs-zoom.legacyPackages.${system}) globalprotect-openconnect;
  };

  quickemuOverlay = f: p: {
    quickemu = p.quickemu.override { qemu_full = p.qemu; };
  };
in
[
  cowsayOverlay
  fishOverlay
  libOverlay
  nix-index.overlays.${system}.default
  nixIndexDatabaseOverlay
  nixSearchOverlay
  metalsOverlay
  megasyncOverlay
  quickemuOverlay
  nurpkgs.overlays.default
  neovim-flake.overlays.${system}.default
  statix.overlays.default
  xargsOverlay
  pipewireOverlay
  globalProtectOverlay
  (import ../home/overlays/bat-lvl)
  (import ../home/overlays/bazecor)
  (import ../home/overlays/hypr-monitor-attached)
  (import ../home/overlays/juno-theme)
  (import ../home/overlays/pyprland)
  (import ../home/overlays/ranger)
  (import ../home/overlays/zoom)
  buildersOverlay
  treesitterGrammarsOverlay
  schemaOverlay
]
