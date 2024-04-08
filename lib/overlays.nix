{ inputs, system }:

with inputs;

let
  cowsayOverlay = f: p: {
    inherit (cowsay.packages.${system}) cowsay;
  };

  fishOverlay = f: p: {
    inherit fish-bobthefish-theme fish-keytool-completions;
  };

  metalsOverlay = f: p: {
    metals = p.callPackage ../home/programs/neovim-ide/metals.nix { };
    metals-updater = p.callPackage ../home/programs/neovim-ide/update-metals.nix { };
  };

  libOverlay = f: p: rec {
    libx = import ./. { inherit (p) lib; };
    lib = p.lib.extend (_: _: {
      inherit (libx) removeNewline secretManager;
    });
  };

  buildersOverlay = f: p: {
    mkHomeConfigurations = { pkgs ? f, extraPkgs ? [ ] }:
      import ../outputs/home-conf.nix { inherit extraPkgs inputs pkgs system; };

    mkNixosConfigurations = { pkgs ? f }:
      import ../outputs/nixos-conf.nix { inherit inputs pkgs system; };
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

  secretsOverlay = f: p: {
    secrets = p.callPackage ./secrets.nix { };
  };

  megasyncOverlay = f: p: {
    inherit (import inputs.nixpkgs-mega {
      inherit system;
      config.allowUnfree = true;
    }) megasync;
  };

  xargsOverlay = f: p: {
    xargs = { hidpi }: {
      inherit hidpi;
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
        mv $out/bin/nix $out/bin/nix-schema
      '';
    });
  };
in
[
  cowsayOverlay
  fishOverlay
  libOverlay
  metalsOverlay
  secretsOverlay
  megasyncOverlay
  nurpkgs.overlay
  neovim-flake.overlays.${system}.default
  statix.overlays.default
  xargsOverlay
  (import ../home/overlays/bat-lvl)
  (import ../home/overlays/bazecor)
  (import ../home/overlays/juno-theme)
  (import ../home/overlays/ranger)
  buildersOverlay
  treesitterGrammarsOverlay
  schemaOverlay
]
