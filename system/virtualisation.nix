{ pkgs, ... }:

# docs: https://nixos.wiki/wiki/NixOS:nixos-rebuild_build-vm
{
  programs.virt-manager.enable = true;

  environment.systemPackages = with pkgs; [ quickemu ];

  # the default network isn't started by default
  systemd.services.libvirt-net-start = {
    description = "Automatically start libvirt default network on boot";
    after = [ "libvirtd.service" ];
    requires = [ "libvirtd.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.libvirt}/bin/virsh net-start default";
      RemainAfterExit = true;
    };
  };

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
    libvirtd.enable = true;
    spiceUSBRedirection.enable = true;

    # nixos-rebuild --build-vm
    vmVariant = {
      virtualisation = {
        memorySize = 8192;
        cores = 6;
        # Assigns 50 GB of disk space
        diskSize = 51200;
      };
    };
  };
}
