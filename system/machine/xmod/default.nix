{ pkgs, lib, specialArgs, ... }:

let
  inherit (specialArgs) inputs;
in
{
  imports = [
    ../tongfang-amd
    inputs.home-manager.nixosModules.home-manager
    # iso image modules
    # FIXME: Package ‘zfs-kernel-2.3.0-6.13.5’ is marked as broken
    #"${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
    # disable networking.wireless from the iso minimal conf as we use networkmanager
    { networking.wireless.enable = false; }
    { networking.hostName = lib.mkForce "xmod-amd"; }
    { nixpkgs.config.allowUnfree = true; }
    # home manager settings
    {
      home-manager = {
        extraSpecialArgs = pkgs.xargs;
        useGlobalPkgs = false;

        sharedModules = [
          inputs.neovim-flake.homeManagerModules.${pkgs.system}.default
          inputs.nix-index.homeManagerModules.${pkgs.system}.default
          ({ nix.registry.nixpkgs.flake = inputs.nixpkgs; })
        ];

        users.gvolpe = import ../../../home/wm/xmonad/home.nix;
      };
    }
  ];
}
