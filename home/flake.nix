{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    nurpkgs = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tex2nix = {
      url = "github:Mic92/tex2nix/4b17bc0";
      inputs.utils.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, nurpkgs, home-manager, tex2nix, ... }: {
    overlays = [
      nurpkgs.overlay
    ];

    homeConfigurations =
      let
        system = "x86_64-linux";

        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;

          # FIXME: should not be set here (see home.nix xdg.enable = true;)
          config.xdg.configHome = "/home/gvolpe/.config";
        };

        nur = import nurpkgs {
          inherit pkgs;
          nurpkgs = pkgs;
        };

        mkHome = conf: (
          inputs.home-manager.lib.homeManagerConfiguration rec {
            inherit system;

            username = "gvolpe";
            homeDirectory = "/home/${username}";

            stateVersion = "21.03";
            configuration = conf;
          });

        edpConf = import ./display/edp.nix {
          inherit nur pkgs;
          inherit (pkgs) config lib stdenv;
        };

        hdmiConf = import ./display/hdmi.nix {
          inherit nur pkgs;
          inherit (pkgs) config lib stdenv;
        };
      in
      {
        gvolpe-edp = mkHome edpConf;
        gvolpe-hdmi = mkHome hdmiConf;
      };
  };
}
