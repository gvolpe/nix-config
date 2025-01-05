{ pkgs, ... }:

# docs: https://nixos.wiki/wiki/NixOS:nixos-rebuild_build-vm
let
  # see https://github.com/NixOS/nixpkgs/pull/370304
  cephPatch = pkgs.fetchpatch {
    url = "https://patch-diff.githubusercontent.com/raw/nixos/nixpkgs/pull/370304.patch";
    sha256 = "sha256-+fx2PxP1Fk4bYVzYP8wWU0bVv2I2fUJ+DL9atYkdupk=";
  };

  quickemu = pkgs.quickemu.override {
    qemu_full = pkgs.qemu_full.override {
      ceph = pkgs.applyPatches {
        src = pkgs.ceph;
        patches = [ cephPatch ];
      };
    };
  };
in
{
  programs.virt-manager.enable = true;

  environment.systemPackages = [ quickemu ];

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
      };
    };
  };
}
