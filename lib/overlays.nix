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

  secretsOverlay = f: p: {
    secrets = p.callPackage ./secrets.nix { };
  };

  xargsOverlay = f: p: {
    xargs = { hidpi }: {
      inherit hidpi;
      inherit (inputs) gh-md-toc penguin-fox;
      inherit (inputs.rycee-nurpkgs.lib.${system}) buildFirefoxXpiAddon;
      addons = f.nur.repos.rycee.firefox-addons;
    };
  };
in
[
  cowsayOverlay
  fishOverlay
  libOverlay
  metalsOverlay
  secretsOverlay
  nurpkgs.overlay
  neovim-flake.overlays.${system}.default
  statix.overlays.default
  xargsOverlay
  (import ../home/overlays/bat-lvl)
  (import ../home/overlays/bazecor)
  (import ../home/overlays/juno-theme)
  (import ../home/overlays/ranger)
  buildersOverlay
]
