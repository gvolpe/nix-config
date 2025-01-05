{ pkgs, ... }:

# docs: https://nixos.wiki/wiki/NixOS:nixos-rebuild_build-vm
{
  programs.virt-manager.enable = true;

  environment.systemPackages = with pkgs; [ quickemu ];

  users = {
    groups.nixosvmtest = { };
    groups.libvirtd.members = [ "gvolpe" ];

    users.vmtest = {
      isSystemUser = true;
      initialPassword = "test";
      group = "nixosvmtest";
    };
  };

  virtualisation = {
    # virtual manager for vms
    libvirtd.enable = false;
    spiceUSBRedirection.enable = false;

    # nixos-rebuild --build-vm
    vmVariant = {
      virtualisation = {
        memorySize = 8192;
        cores = 6;
      };
    };
  };
}
