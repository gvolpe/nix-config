{ inputs, system, ... }:

with inputs;

let
  cowsayOverlay = f: p: {
    inherit (inputs.cowsay.packages.${system}) cowsay;
  };

  fishOverlay = f: p: {
    inherit fish-bobthefish-theme fish-keytool-completions;
  };

  nautilusOverlay = f: p: {
    nautilus-gtk3 = nixpkgs-nautilus-gtk3.legacyPackages.${system}.gnome.nautilus;
  };

  libOverlay = f: p: rec {
    libx = import ../lib { inherit (p) lib; };
    lib = p.lib.extend (_: _: {
      inherit (libx) removeNewline secretManager;
    });
  };

  sxmOverlay =
    if (builtins.hasAttr "sxm-flake" inputs)
    then sxm-flake.overlays.default
    else (f: p: { });

  pkgs = import nixpkgs {
    inherit system;

    config.allowUnfree = true;

    overlays = [
      cowsayOverlay
      fishOverlay
      libOverlay
      nautilusOverlay
      nurpkgs.overlay
      neovim-flake.overlays.${system}.default
      statix.overlays.default
      sxmOverlay
      (import ../home/overlays/nautilus)
      (import ../home/overlays/ranger)
    ];
  };

  nur = import nurpkgs {
    inherit pkgs;
    nurpkgs = pkgs;
  };

  imports = [
    neovim-flake.nixosModules.${system}.hm
    ../home/home.nix
  ];

  mkHome = { hidpi ? false }: (
    home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

      extraSpecialArgs = {
        inherit hidpi;
        inherit (inputs) gh-md-toc;
        inherit (rycee-nurpkgs.lib.${system}) buildFirefoxXpiAddon;
        addons = nur.repos.rycee.firefox-addons;
      };

      modules = [{ inherit imports; }];
    }
  );
in
{
  gvolpe-edp = mkHome { hidpi = false; };
  gvolpe-hdmi = mkHome { hidpi = true; };
}
