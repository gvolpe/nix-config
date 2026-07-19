{ config, lib, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;

  # gram needs nodejs to dynamically discover the system architecture
  binaries = with pkgs;[ metals nodejs ];

  gram = pkgs.symlinkJoin {
    name = "gram-wrapped";
    paths = [ pkgs.gram ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/gram \
        --prefix PATH : ${lib.makeBinPath binaries}
    '';
  };

  extensions = pkgs.callPackage ./extensions { };
  extensions-dir = pkgs.gram-ext.linkGramExtensions extensions;

  configPath = "${config.dotfiles.path}/programs/gram";

  mkConfigFile = filepath: filename:
    if !config.dotfiles.mutable then filepath
    else mkOutOfStoreSymlink "${configPath}/${filename}";

  keymapFile = mkConfigFile ./keymap.jsonc "keymap.jsonc";
  settingsFile = mkConfigFile ./settings.jsonc "settings.jsonc";
in
{
  home.packages = [ gram ];

  xdg.configFile."gram/keymap.jsonc".source = keymapFile;
  xdg.configFile."gram/settings.jsonc".source = settingsFile;

  xdg.dataFile."gram/extensions/installed" = {
    enable = true;
    source = extensions-dir;
    onChange = ''
      cd "${config.xdg.dataHome}/gram/extensions"
      mv index.json index.json.backup
    '';
  };
}
