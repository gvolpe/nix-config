{ ... }:

# docs: https://nixos.wiki/wiki/NixOS:nixos-rebuild_build-vm
{
  users = {
    groups.nixosvmtest = { };

    users.vmtest = {
      isSystemUser = true;
      initialPassword = "test";
      group = "nixosvmtest";
    };
  };

  virtualisation.vmVariant = {
    virtualisation = {
      memorySize = 4096;
      cores = 4;
    };
  };
}
