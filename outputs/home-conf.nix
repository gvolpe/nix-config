{ inputs, system, ... }:

with inputs;

let
  fishOverlay = f: p: {
    inherit fish-bobthefish-theme fish-keytool-completions;
  };

  nautilusOverlay = f: p: {
    nautilus-gtk3 = nixpkgs-nautilus-gtk3.legacyPackages.${system}.gnome.nautilus;
  };

  pkgs = import nixpkgs {
    inherit system;

    config.allowUnfree = true;

    overlays = [
      fishOverlay
      nautilusOverlay
      nurpkgs.overlay
      neovim-flake.overlays.${system}.default
      (f: p: { tex2nix = tex2nix.defaultPackage.${system}; })
      (import ../home/overlays/nautilus)
      (import ../home/overlays/ranger)
      (import ../home/overlays/xdg-open)
    ];
  };

  nur = import nurpkgs {
    inherit pkgs;
    nurpkgs = pkgs;
  };

  imports = [
    homeage.homeManagerModules.homeage
    neovim-flake.nixosModules.${system}.hm
    ../home/home.nix
  ];

  mkHome = { hidpi ? false }: (
    home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

      extraSpecialArgs = {
        inherit hidpi;
        addons = nur.repos.rycee.firefox-addons;
        gh-md-toc = inputs.gh-md-toc;
      };

      modules = [{ inherit imports; }];
    }
  );
in
{
  gvolpe-edp = mkHome { hidpi = false; };
  gvolpe-hdmi = mkHome { hidpi = true; };

  # Continuos Integration automation
  ci = {
    metals = pkgs.callPackage ../home/programs/neovim-ide/metals.nix { };
    metals-updater = pkgs.callPackage ../home/programs/neovim-ide/update-metals.nix { };
  };
}
