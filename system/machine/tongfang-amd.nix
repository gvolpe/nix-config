{ config, pkgs, ... }:

{
  services.xserver.videoDrivers = [ "amdgpu" ];
}
