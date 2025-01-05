{ config, pkgs, ... }:

let
  # https://nixos.org/manual/nixpkgs/unstable/#sec-checkpoint-build
  inherit (pkgs.checkpointBuildTools) prepareCheckpointBuild mkCheckpointBuild;

  xdgCheckpoint = prepareCheckpointBuild pkgs.xdg-utils;

  # xdg-utils as a runtime dependency: https://github.com/NixOS/nixpkgs/pull/181171
  # see on postFixup instead of postInstall: https://github.com/NixOS/nixpkgs/pull/285233
  xdgAsMimeo = pkgs.xdg-utils.overrideAttrs (old: {
    postFixup = ''
      cp ${pkgs.mimeo}/bin/mimeo $out/bin/xdg-open
    '';
  });

  xdg-mimeo = mkCheckpointBuild xdgAsMimeo xdgCheckpoint;
in
{
  home.packages = [ pkgs.mimeo xdg-mimeo ];
  xdg.configFile."mimeo/associations.txt".text = config.secrets.mimeoAssociations;
}
