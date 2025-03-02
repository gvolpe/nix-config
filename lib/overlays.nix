{ inputs, system }:

let
  # nixos-version needs this to work with flakes
  libVersionOverlay = import "${inputs.nixpkgs}/lib/flake-version-info.nix" inputs.nixpkgs;

  libOverlay = f: p: rec {
    libx = import ./. { inherit (p) lib; };
    lib = (p.lib.extend (_: _: {
      inherit (libx) exe removeNewline secretManager;
    })).extend libVersionOverlay;
  };

  overlays = f: p: {
    inherit (inputs.cowsay.packages.${system}) cowsay;
    inherit (inputs) fish-bobthefish-theme fish-keytool-completions;

    inherit (inputs.nix-index-database.packages.${system}) nix-index-database nix-index-small-database;

    # globalprotect vpn overlay for no-longer supported package
    inherit (inputs.nixpkgs-zoom.legacyPackages.${system}) globalprotect-openconnect;

    inherit (import inputs.nixpkgs-mega { inherit system; config.allowUnfree = true; }) megasync;

    # firefox addon builder function
    inherit (inputs.rycee-nurpkgs.lib.${system}) buildFirefoxXpiAddon;

    builders = {
      mkHome = { pkgs ? f, extraHomeConfig ? { } }:
        import ../outputs/hm.nix { inherit extraHomeConfig inputs pkgs system; };

      mkNixos = { pkgs ? f, extraSystemConfig ? { } }:
        import ../outputs/os.nix { inherit extraSystemConfig inputs pkgs system; };
    };

    nix-search = inputs.nix-search.packages.${system}.default;

    metals = p.callPackage ../home/programs/neovim-ide/metals.nix { };
    metals-updater = p.callPackage ../home/programs/neovim-ide/update-metals.nix { };

    nix-schema = inputs.nix-schema.packages.${system}.nix.overrideAttrs (old: {
      doCheck = false;
      doInstallCheck = false;
      postInstall = old.postInstall + ''
        rm $out/bin/nix-*
        mv $out/bin/nix $out/bin/nix-schema
      '';
    });

    # pipewire overlay for broken zoom-us
    pipewire-zoom = inputs.nixpkgs-zoom.legacyPackages.${system}.pipewire;

    treesitterGrammars = ts: ts.withPlugins (p: [
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
      p.tree-sitter-tsx
      p.tree-sitter-typescript
      p.tree-sitter-html
      p.tree-sitter-javascript
      p.tree-sitter-css
      p.tree-sitter-graphql
      p.tree-sitter-json
      p.tree-sitter-smithy
    ]);

    xargs = {
      inherit (inputs) gh-md-toc penguin-fox;
      addons = f.nur.repos.rycee.firefox-addons;
    };
  };
in
[
  libOverlay
  overlays
  inputs.nix-index.overlays.${system}.default
  inputs.nurpkgs.overlays.default
  inputs.neovim-flake.overlays.${system}.default
  inputs.statix.overlays.default
  (import ../home/overlays/bat-lvl)
  (import ../home/overlays/bazecor)
  (import ../home/overlays/hypr-monitor-attached)
  (import ../home/overlays/juno-theme)
  (import ../home/overlays/pyprland)
  (import ../home/overlays/ranger)
  (import ../home/overlays/zoom)
]
