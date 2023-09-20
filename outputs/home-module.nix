{ inputs, system, extraSpecialArgs, ... }:

{
  home-manager = {
    inherit extraSpecialArgs;
    useGlobalPkgs = true;

    sharedModules = [
      inputs.neovim-flake.nixosModules.${system}.hm
    ];

    users.ali = import ../home/home.nix;
  };
}
