{ config, lib, pkgs, ... }:

let
  inherit (config) dotfiles;

  # debugger extension for Rust (among others)
  codeLLDB = pkgs.vscode-extensions.vadimcn.vscode-lldb;
  codeLLDBAdapter = "${codeLLDB}/share/vscode/extensions/vadimcn.vscode-lldb/adapter/codelldb";

  binaries = [
    pkgs.metals # scala lsp server
    pkgs.nil # nix lsp server
    pkgs.nodejs # required by gram
    pkgs.rust-analyzer # rust
  ];

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

  # rust debugger symlink
  home.file.".local/bin/gram-codelldb".source = codeLLDBAdapter;

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
