{ inputs, system, ... }:

with inputs;

let
  username = "gvolpe";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";

  fishOverlay = f: p: {
    inherit (inputs) fish-bobthefish-theme fish-keytool-completions;
  };

  pkgs = import nixpkgs {
    inherit system;

    config.allowUnfree = true;
    config.xdg.configHome = configHome;

    overlays = [
      fishOverlay
      nurpkgs.overlay
      neovim-flake.overlays.default
      (f: p: { tex2nix = tex2nix.defaultPackage.${system}; })
      ((import ../home/overlays/md-toc) { inherit (inputs) gh-md-toc; })
      (import ../home/overlays/protonvpn-gui)
      (import ../home/overlays/ranger)
    ];
  };

  nur = import nurpkgs {
    inherit pkgs;
    nurpkgs = pkgs;
  };

  mkHome = { hidpi ? false }: (
    home-manager.lib.homeManagerConfiguration rec {
      inherit pkgs;

      extraSpecialArgs = {
        inherit hidpi;
        addons = nur.repos.rycee.firefox-addons;
      };

      modules = [
        {
          imports = [
            neovim-flake.nixosModules.hm
            ../home/home.nix
          ];
          home = {
            inherit username homeDirectory;
            stateVersion = "21.03";
          };
        }
      ];
    }
  );
in
{
  gvolpe-edp = mkHome { hidpi = false; };
  gvolpe-hdmi = mkHome { hidpi = true; };
}
