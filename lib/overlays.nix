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

  # needs to be imported after the neovim-flake overlays
  metalsOverlay = f: p: {
    metals = p.callPackage ../home/programs/neovim-ide/metals.nix { };
    metals-updater = p.callPackage ../home/programs/neovim-ide/update-metals.nix { };
  };

  overlays = f: p: {
    inherit (inputs.cowsay.packages.${system}) cowsay;
    inherit (inputs) fish-bobthefish-theme fish-keytool-completions;
    inherit (inputs.snitch.packages.${system}) snitch;

    inherit (inputs.nfsm-flake.packages.${system}) nfsm nfsm-cli;
    inherit (inputs.niri-scratchpad-flake.packages.${system}) niri-scratchpad;
    inherit (inputs.nsticky-flake.packages.${system}) nsticky;

    inherit (inputs.nix-index-database.packages.${system}) nix-index-database nix-index-small-database;
    inherit (inputs.nixpkgs-hyprland.legacyPackages.${system}) hyprland;

    # firefox addon builder function
    inherit (inputs.rycee-nurpkgs.lib.${system}) buildFirefoxXpiAddon;

    builders = {
      mkHome = { pkgs ? f, extraHomeConfig ? { } }:
        import ../outputs/hm.nix { inherit extraHomeConfig inputs pkgs system; };

      mkNixos = { pkgs ? f, extraSystemConfig ? { } }:
        import ../outputs/os.nix { inherit extraSystemConfig inputs pkgs system; };
    };

    nix-search = inputs.nix-search.packages.${system}.default;

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

    wooz = inputs.wooz-flake.packages.${system}.default;

    # gram v3.0.1
    gram = inputs.nixpkgs-gram.legacyPackages.${system}.gram;

    gram-ext = {
      inherit (inputs.gram-extensions.packages.${system})
        buildGramExtension buildGramRustExtension linkGramExtensions
        bearded-icons catppuccin catppuccin-icons git-firefly night-owlz;
    };

    sources = {
      inherit (inputs)
        determinate-nix diskonaut gh-md-toc hyprlax hypr-monitor-attached pyprland
        metals-zed tree-sitter-scala pedantix waycal wshowkeys;
    };

    xargs = {
      inherit (inputs) nord-tmux;
      addons = f.nur.repos.rycee.firefox-addons;
    };
  };
in
[
  libOverlay
  overlays
  inputs.helium-nix.overlays.default
  inputs.neovim-flake.overlays.default
  inputs.nix-index.overlays.default
  inputs.nurpkgs.overlays.default
  inputs.niri-flake.overlays.niri
  inputs.vicinae.overlays.default
  metalsOverlay
  (import ../home/overlays/bazecor)
  (import ../home/overlays/determinate-nix)
  (import ../home/overlays/diskonaut)
  (import ../home/overlays/gh-md-toc)
  (import ../home/overlays/handy)
  (import ../home/overlays/hyprlax)
  (import ../home/overlays/hypr-monitor-attached)
  (import ../home/overlays/pedantix)
  (import ../home/overlays/pyprland)
  (import ../home/overlays/sway-audio-idle-inhibit)
  (import ../home/overlays/scripts)
  (import ../home/overlays/waycal)
  (import ../home/overlays/wshowkeys)
]
