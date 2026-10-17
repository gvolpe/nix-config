{ inputs, system }:

let
  # nixos-version needs this to work with flakes
  libVersionOverlay = import "${inputs.nixpkgs}/lib/flake-version-info.nix" inputs.nixpkgs;

  libOverlay = f: p: rec {
    libx = import ./. { inherit (p) lib; pkgs = f; };
    lib = (p.lib.extend (_: _: {
      inherit (libx) exe removeNewline secretManager mkWrapper;
    })).extend libVersionOverlay;
  };

  overlays = f: p: {
    inherit (inputs) fish-bobthefish-theme fish-keytool-completions;
    inherit (inputs.snitch.packages.${system}) snitch;

    inherit (inputs.nfsm-flake.packages.${system}) nfsm nfsm-cli;
    inherit (inputs.niri-scratchpad-flake.packages.${system}) niri-scratchpad;
    inherit (inputs.nsticky-flake.packages.${system}) nsticky;

    inherit (inputs.nix-index-database.packages.${system}) nix-index-database nix-index-small-database;
    inherit (inputs.nix-graph.packages.${system}) nix-graph;

    # firefox addon builder function
    inherit (inputs.rycee-nurpkgs.lib.${system}) buildFirefoxXpiAddon;
    inherit (f.nur.repos.rycee) firefox-addons;

    # https://github.com/NixOS/nixpkgs/pull/498572
    helium =
      let
        heliumPkgs = inputs.nixpkgs-helium.legacyPackages.${system};
        upstream = heliumPkgs.helium;
      in
      upstream.override {
        newScope = scope: heliumPkgs.newScope (scope // {
          mkChromiumDerivation = args: (upstream.mkDerivation args).overrideAttrs (old: {
            # Helium applies its own system-Python patch later in postPatch.
            patches = p.lib.filter (patch: !(p.lib.hasSuffix
              "chromium-153-revert-DEPS-use-DEPS-provided-python-instead-of-a-system-one.patch"
              (toString patch))) old.patches;
            # Temporary workaround: Helium's pruning also removes the TypeScript patch.
            prePatch = (old.prePatch or "") + ''
              cp third_party/typescript/linux-amd64/3pp/patches/typescript_native_preview.patch \
                "$TMPDIR/helium-typescript-native-preview.patch"
            '';
            postPatch = p.lib.replaceStrings
              [ "--input=../3pp/patches/typescript_native_preview.patch" ]
              [ ''--input="$TMPDIR/helium-typescript-native-preview.patch"'' ]
              old.postPatch;
            # Chromium 154+ replaced these placeholders with desktop_icon/desktop_exec.
            installPhase = p.lib.concatStringsSep "\n" (p.lib.filter (line:
              !(p.lib.hasInfix ''--replace-fail "@@PACKAGE"'' line
                || p.lib.hasInfix ''--replace-fail "/usr/bin/@@usr_bin_symlink_name"'' line)
            ) (p.lib.splitString "\n" old.installPhase));
          });
        });
      };

    neovim = inputs.mnw-flake.packages.${system}.default;
    neovim-dev = inputs.mnw-flake.packages.${system}.dev;
    neovim-nightly = inputs.mnw-flake.packages.${system}.nightly;

    nix-search = inputs.nix-search.packages.${system}.default;

    treesitterGrammars = ts: ts.withPlugins (p: [
      p.tree-sitter-scala
      p.tree-sitter-c
      p.tree-sitter-nix
      p.tree-sitter-elm
      p.tree-sitter-lua
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

    wl-party = inputs.wl-party.packages.${system}.default;
    wooz = inputs.wooz-flake.packages.${system}.default;

    gram-ext = {
      inherit (inputs.gram-extensions.packages.${system})
        buildGramExtension buildGramRustExtension linkGramExtensions
        bearded-icons catppuccin catppuccin-icons git-firefly night-owlz;
    };

    sources = {
      inherit (inputs) determinate-nix gh-md-toc metals-zed nord-tmux
      nirimation niri-shaders tree-sitter-scala pedantix waycal wshowkeys;
    };
  };
in
[
  libOverlay
  overlays
  inputs.latchshot.overlays.default
  inputs.niri.overlays.default
  inputs.nix-index.overlays.default
  inputs.nurpkgs.overlays.default
  inputs.syncrain.overlays.default
  (import ../home/overlays/bazecor)
  (import ../home/overlays/determinate-nix)
  (import ../home/overlays/gh-md-toc)
  (import ../home/overlays/handy)
  (import ../home/overlays/pedantix)
  (import ../home/overlays/sway-audio-idle-inhibit)
  (import ../home/overlays/scripts)
  (import ../home/overlays/waycal)
  (import ../home/overlays/wshowkeys)
]
