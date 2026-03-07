{ pkgs, lib, config, ... }:

let
  filePath = "${config.dotfiles.path}/programs/neofetch/electric.conf";
  configSrc =
    if !config.dotfiles.mutable then ./electric.conf
    else config.lib.file.mkOutOfStoreSymlink filePath;

  fastfetchPath = lib.makeBinPath (with pkgs; [ chafa imagemagick ]);

  fastfetch = pkgs.fastfetch.overrideAttrs (old: {
    postInstall = ''
      substituteInPlace $out/bin/fastfetch --add-flags "--chafa ${./nixos.png}"
      wrapProgram $out/bin/fastfetch --prefix PATH : ${fastfetchPath}
    '';
  });
in
{
  home.packages = [ fastfetch ];
  #xdg.configFile."hyfetch.json".source = ./hyfetch.json;
  #xdg.configFile."neofetch/config.conf".source = configSrc;
}
