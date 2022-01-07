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

  outputs = inputs @ { self, nixpkgs, nurpkgs, home-manager, ... }: {
    homeConfigurations =
      let
        system = "x86_64-linux";

        pkgs = import nixpkgs {
          inherit system;

          config.allowUnfree = true;

          # FIXME: should not be set here (see home.nix xdg.enable = true;)
          config.xdg.configHome = "/home/gvolpe/.config";

          overlays = [
            nurpkgs.overlay
            (f: p: { tex2nix = inputs.tex2nix.defaultPackage.${system}; })
          ];
        };

        nur = import nurpkgs {
          inherit pkgs;
          nurpkgs = pkgs;
        };

        mkHome = conf: (
          inputs.home-manager.lib.homeManagerConfiguration rec {
            inherit pkgs system;

            username = "gvolpe";
            homeDirectory = "/home/${username}";

            stateVersion = "21.03";
            configuration = conf;
          });

        edpConf = import ./home/display/edp.nix {
          inherit nur pkgs;
          inherit (pkgs) config lib stdenv;
        };

        hdmiConf = import ./home/display/hdmi.nix {
          inherit nur pkgs;
          inherit (pkgs) config lib stdenv;
        };
      in
      {
        gvolpe-edp = mkHome edpConf;
        gvolpe-hdmi = mkHome hdmiConf;
      };

    nixosConfigurations = {
      dell-xps = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./system/machine/dell-xps.nix
          ./system/configuration.nix
        ];
      };

      tongfang-amd = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./system/machine/tongfang-amd.nix
          ./system/configuration.nix
        ];
      };
    };

  };
}
