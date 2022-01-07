{ system, nixpkgs, nurpkgs, home-manager, tex2nix, ... }:

let
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;

    # FIXME: should not be set here (see home.nix xdg.enable = true;)
    config.xdg.configHome = "/home/gvolpe/.config";

    overlays = [
      nurpkgs.overlay
      (f: p: { tex2nix = tex2nix.defaultPackage.${system}; })
      (import ../home/overlays/md-toc)
    ];
  };

  nur = import nurpkgs {
    inherit pkgs;
    nurpkgs = pkgs;
  };

  mkHome = conf: (
    home-manager.lib.homeManagerConfiguration rec {
      inherit pkgs system;

      username = "gvolpe";
      homeDirectory = "/home/${username}";

      stateVersion = "21.03";
      configuration = conf;
    });

  edpConf = import ../home/display/edp.nix {
    inherit nur pkgs;
    inherit (pkgs) config lib stdenv;
  };

  hdmiConf = import ../home/display/hdmi.nix {
    inherit nur pkgs;
    inherit (pkgs) config lib stdenv;
  };
in
{
  gvolpe-edp = mkHome edpConf;
  gvolpe-hdmi = mkHome hdmiConf;
}
