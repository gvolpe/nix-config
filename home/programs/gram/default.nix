{ config, lib, pkgs, ... }:

let
  inherit (config) dotfiles;

  # gram needs nodejs to dynamically discover the system architecture
  binaries = with pkgs;[ metals nil nodejs ];

  gram = pkgs.symlinkJoin {
    name = "gram-wrapped";
    nativeBuildInputs = [ pkgs.makeWrapper ];
    paths = [ pkgs.gram ];
    postBuild = ''
      wrapProgram $out/bin/gram \
        --prefix PATH : ${lib.makeBinPath binaries}
    '';
  };

  extensions = pkgs.callPackage ./extensions { };
  extensions-dir = pkgs.gram-ext.linkGramExtensions extensions;

  keymapFile = dotfiles.make ./keymap.jsonc ./.;
  settingsFile = dotfiles.make ./settings.jsonc ./.;
in
{
  home.packages = [ gram ];

  xdg.configFile."gram/keymap.jsonc".source = keymapFile;
  xdg.configFile."gram/settings.jsonc".source = settingsFile;

  xdg.dataFile."gram/extensions/installed" = {
    enable = true;
    onChange = ''
      cd "${config.xdg.dataHome}/gram/extensions"
      mv index.json index.json.backup
    '';
    source = extensions-dir;
  };
}
