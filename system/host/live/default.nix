{ pkgs, lib, modulesPath, specialArgs, ... }:

let
  inherit (specialArgs) inputs;
in
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ../../wm/niri.nix
    inputs.home-manager.nixosModules.home-manager
  ];

  home-manager = {
    extraSpecialArgs = pkgs.xargs;
    useGlobalPkgs = true;
    users.gvolpe = {
      hidpi = true;

      imports = [
        inputs.dots.homeModules.default
        inputs.neovim-flake.homeModules.default
        inputs.nix-index.homeModules.default
        (import ../../../home/dotfiles.nix { mutable = false; })
        inputs.sunix.homeModules.default
        ../../../home/wm/niri
      ];

      nix.registry.nixpkgs.flake = inputs.nixpkgs;
    };
  };

  networking = {
    hostName = "live";
    useDHCP = lib.mkDefault true;
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  system.stateVersion = "26.11";

  # overrides for system/configuration.nix (imported in outputs/os.nix)
  virtualisation.docker.enable = lib.mkForce false;
  hardware.sane.enable = lib.mkForce false;

  security.pam.yubico.enable = lib.mkForce false;

  system.activationScripts.diff = lib.mkForce "";

  services = {
    avahi.enable = lib.mkForce false;
    gvfs.enable = lib.mkForce false;
    pcscd.enable = lib.mkForce false;
    printing.enable = lib.mkForce false;
    udev.packages = lib.mkForce [ ];
  };
}
