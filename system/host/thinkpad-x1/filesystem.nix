{
  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/626461ae-d544-4eea-81a4-2e1cf3b8fea3";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/8429-74E0";
      fsType = "vfat";
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/abdc1933-db67-4ff6-8e3b-034c9beb54a9"; }];
}
