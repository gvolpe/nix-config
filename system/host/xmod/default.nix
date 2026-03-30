{ pkgs, lib, specialArgs, ... }:

let
  inherit (specialArgs) inputs;
  inherit (pkgs.stdenv.hostPlatform) system;
in
{
  imports = [
    ./hardware-configuration.nix
    inputs.home-manager.nixosModules.home-manager
  ];

  home-manager = {
    extraSpecialArgs = pkgs.xargs;
    useGlobalPkgs = true;

    sharedModules = [
      inputs.neovim-flake.homeManagerModules.${system}.default
      inputs.nix-index.homeManagerModules.${system}.default
      ({ nix.registry.nixpkgs.flake = inputs.nixpkgs; })
      { hidpi = false; dotfiles.mutable = false; }
    ];

    users.gvolpe = import ./home.nix;
  };

  environment.systemPackages = with pkgs; [
    gnome-console
    globalprotect-openconnect
  ];

  programs.dconf.enable = true;

  services = {
    dbus.enable = true;
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;

    globalprotectvpn.enable = true;

    gnome = {
      core-apps.enable = false;
      core-developer-tools.enable = false;
      games.enable = false;
    };
  };

  networking.hostName = "nixos-vm";

  users.users.gvolpe = {
    initialPassword = lib.mkForce "test";
    extraGroups = lib.mkForce [ "libvirtd" "networkmanager" "wheel" ];
    linger = lib.mkForce true;
  };

  # wayland compositors require 3d acceleration
  virtualisation.vmVariant = {
    hardware.graphics = {
      enable = true;
      extraPackages = [ pkgs.mesa ];
    };

    services = {
      qemuGuest.enable = true;
      spice-autorandr.enable = true;
      spice-vdagentd.enable = true; # enable copy and paste between host and guest
    };

    virtualisation = {
      diskSize = 51200; # 50 gbs
      graphics = true;

      sharedDirectories = {
        sxm_workspace = {
          source = "/home/gvolpe/workspace/sxm";
          target = "/mnt/workspace";
        };
      };

      qemu.options = [
        "-device virtio-vga-gl"
        "-display gtk,gl=on"
        "-device virtio-keyboard-pci"
        "-usb"
        "-device usb-tablet"
      ];
    };

    boot = {
      initrd.kernelModules = [ "virtio_gpu" ];
      kernelModules = [ "virtio_gpu" ];
      kernelPackages = pkgs.linuxPackages_latest;
      loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = true;
      };
    };

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
  };

  system.stateVersion = "23.05";
}
