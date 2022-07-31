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
      modules = [
        {
          imports = [ ../home/home.nix ];

          programs = {
            firefoxie = {
              enable = true;
              addons = nur.repos.rycee.firefox-addons;
              inherit hidpi;
            };

            megasync = {
              enable = true;
              inherit hidpi;
            };

            polybar = {
              enable = true;
              inherit hidpi;
            };

            spotify = {
              enable = true;
              inherit hidpi;
            };

            termie = {
              enable = true;
              inherit hidpi;
            };

            xmonad = {
              enable = true;
              inherit hidpi;
            };
          };
        }
        {
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
