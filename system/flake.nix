{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = inputs: {

    nixosConfigurations = {
      dell-xps = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./machine/dell-xps.nix
          ./configuration.nix
        ];
      };

      tongfang-amd = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./machine/tongfang-amd.nix
          ./configuration.nix
        ];
      };
    };

  };
}
