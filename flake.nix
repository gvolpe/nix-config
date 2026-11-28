{
  description = "gvolpe's Home Manager & NixOS configurations";

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://cache.manic.systems"
      "https://gvolpe-nixos.cachix.org"
      "https://helium-flake.cachix.org"
      "https://install.determinate.systems"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "gvolpe-nixos.cachix.org-1:0MPlBIMwYmrNqoEaYTox15Ds2t1+3R+6Ycj0hZWMcL0="
      "helium-flake.cachix.org-1:ZNsLgOtylmV74bm0vOWzsu39LNSSIw90vIFVUNbYnJA="
      "cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="
      "cache.manic.systems-1:s6OZanN8Us8vRi0jVivP3qlMn0cYHBjBALKrNe5nH8s="
    ];
  };

  outputs = args @ { self, ... }:
    let
      system = "x86_64-linux";
      inputs = (import ./.tack) { overrides = args.tackOverrides or { }; };
      overlays = import ./lib/overlays.nix { inherit inputs system; };

      pkgs = import inputs.nixpkgs {
        inherit overlays system;
        config = {
          allowUnfree = true;
          contentAddressedByDefault = false;
        };
      };

      inherit (self.homeConfigurations.niri.config.wrappers) fastfetch;
    in
    {
      lib = {
        mkHome = { extraHomeConfig ? { }, pkgs }:
          import ./outputs/hm.nix { inherit inputs pkgs extraHomeConfig; };

        mkNixos = { extraSystemConfig ? { }, pkgs }:
          import ./outputs/os.nix { inherit inputs pkgs extraSystemConfig; };
      };

      homeConfigurations = self.lib.mkHome { inherit pkgs; };
      nixosConfigurations = self.lib.mkNixos { inherit pkgs; };

      out = { inherit pkgs overlays; };

      schemas =
        inputs.flake-schemas.schemas //
        import ./lib/schemas.nix { inherit (inputs) flake-schemas; };

      packages.${system} = {
        inherit fastfetch;
        inherit (inputs.determinate-nix.packages.${system}) nix;
        inherit (pkgs) bazecor helium neovim-dev quickemu slack;
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          pkgs.cachix
          # cd home/secrets && agenix -e new-secret.age
          inputs.agenix.packages.${system}.default
        ];
      };
    };
}
