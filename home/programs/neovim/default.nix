{ config, lib, pkgs, ... }:

let
  custom-plugins = pkgs.callPackage ./custom-plugins.nix {
    inherit (pkgs.vimUtils) buildVimPlugin;
  };

  plugins = pkgs.vimPlugins // custom-plugins;

  overriddenPlugins = with pkgs; [];

  myVimPlugins = with plugins; [
    asyncrun-vim            # run async commands, show result in quickfix window
    coc-nvim                # LSP client + autocompletion plugin
    coc-metals              # Scala LSP client for CoC
    coc-yank                # yank plugin for CoC
    ctrlsf-vim              # edit file in place after searching with ripgrep
    dhall-vim               # Syntax highlighting for Dhall lang
    fzf-hoogle              # search hoogle with fzf
    fzf-vim                 # fuzzy finder
    ghcid                   # ghcid for Haskell
    lightline-vim           # configurable status line (can be used by coc)
    material-vim            # modern theme with true colors support
    multiple-cursors        # Multiple cursors selection, etc
    neomake                 # run programs asynchronously and highlight errors
    nerdcommenter           # code commenter
    nerdtree                # tree explorer
    nerdtree-git-plugin     # shows files git status on the NerdTree
    quickfix-reflector-vim  # make modifications right in the quickfix window
    rainbow_parentheses-vim # for nested parentheses
    tender-vim              # a clean dark theme
    vim-airline             # bottom status bar
    vim-airline-themes
    vim-css-color           # preview css colors
    vim-devicons            # dev icons shown in the tree explorer
    vim-easy-align          # alignment plugin
    vim-easymotion          # highlights keys to move quickly
    vim-fish                # fish shell highlighting
    vim-fugitive            # git plugin
    vim-gtfo                # go to terminal or file manager
    vim-nix                 # nix support (highlighting, etc)
    vim-repeat              # repeat plugin commands with (.)
    vim-ripgrep             # blazing fast search using ripgrep
    vim-scala               # scala plugin
    vim-surround            # quickly edit surroundings (brackets, html tags, etc)
    vim-tmux                # syntax highlighting for tmux conf file and more
  ] ++ overriddenPlugins;

  baseConfig    = builtins.readFile ./config.vim;
  cocConfig     = builtins.readFile ./coc.vim;
  cocSettings   = builtins.toJSON (import ./coc-settings.nix);
  pluginsConfig = builtins.readFile ./plugins.vim;
  vimConfig     = baseConfig + pluginsConfig + cocConfig;

  # neovim-5 nightly stuff
  neovim-5     = pkgs.callPackage ./dev/nightly.nix {};
  nvim5-config = builtins.readFile ./dev/metals.vim;
  new-plugins  = pkgs.callPackage ./dev/plugins.nix {
    inherit (pkgs.vimUtils) buildVimPlugin;
    inherit (pkgs) fetchFromGitHub;
  };
  nvim5-plugins = with new-plugins; [
    completion-nvim
    diagnostic-nvim
    nvim-lsp
    nvim-metals
  ];
in
{
  programs.neovim = {
    enable       = true;
    extraConfig  = vimConfig;
    package      = neovim-5;
    plugins      = myVimPlugins;
    viAlias      = true;
    vimAlias     = true;
    vimdiffAlias = true;
    withNodeJs   = true; # for coc.nvim
    withPython   = true; # for plugins
    withPython3  = true; # for plugins
  };

  xdg.configFile = {
    "nvim/coc-settings.json".text = cocSettings;
  };
}
