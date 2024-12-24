{ inputs, system, extraSpecialArgs, ... }:

{
  home-manager = {
    inherit extraSpecialArgs;
    useGlobalPkgs = true;

    sharedModules = [
      inputs.neovim-flake.homeManagerModules.${system}.default
      inputs.nix-index.homeManagerModules.${system}.default
      ({ nix.registry.nixpkgs.flake = inputs.nixpkgs; })
    ];

    users.gvolpe = import ../home/wm/xmonad/home.nix;
  };
}
