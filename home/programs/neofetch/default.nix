{ pkgs, lib, config, ... }:

let
  filePath = "${config.dotfiles.path}/programs/neofetch/electric.conf";
  configSrc =
    if !config.dotfiles.mutable then ./electric.conf
    else config.lib.file.mkOutOfStoreSymlink filePath;

  neofetchPath = lib.makeBinPath (with pkgs; [ chafa imagemagick ]);

  neofetchSixelsSupport = pkgs.neofetch.overrideAttrs (old: {
    # --add-flags "--source=./nixos.png" doesn't work ¯\_(ツ)_/¯
    postInstall = lib.optionalString (!config.dotfiles.mutable) ''
      substituteInPlace $out/bin/neofetch \
        --replace "image_source=\"auto\"" "image_source=\"${./nixos.png}\""
    '' + ''
      wrapProgram $out/bin/neofetch --prefix PATH : ${neofetchPath}
    '';
  });
in
{
  home.packages = [ pkgs.hyfetch neofetchSixelsSupport ];
  xdg.configFile."hyfetch.json".source = ./hyfetch.json;
  xdg.configFile."neofetch/config.conf".source = configSrc;
}
