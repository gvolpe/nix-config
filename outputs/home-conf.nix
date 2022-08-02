{ system, nixpkgs, nurpkgs, home-manager, tex2nix, ... }:

let
  username = "gvolpe";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";

  pkgs = import nixpkgs {
    inherit system;

    config.allowUnfree = true;
    config.xdg.configHome = configHome;

    overlays = [
      nurpkgs.overlay
      (f: p: { tex2nix = tex2nix.defaultPackage.${system}; })
      (import ../home/overlays/md-toc)
      (import ../home/overlays/nheko)
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
          imports = [ ../home/home.nix ];
          home = {
            inherit username homeDirectory;
            stateVersion = "21.03";
          };
        }
      ];
    });
in
{
  gvolpe-edp = mkHome { hidpi = false; };
  gvolpe-hdmi = mkHome { hidpi = true; };
}
