{ config, pkgs, ... }:

let
  plugins = pkgs.vimPlugins // pkgs.callPackage ./custom-plugins.nix {};

  vimPlugins = with plugins; [
    asyncrun-vim            # run async commands, show result in quickfix window
    coc-metals              # Scala LSP client for CoC
    coc-nvim                # LSP client + autocompletion plugin
    coc-yank                # yank plugin for CoC
    ctrlsf-vim              # edit file in place after searching with ripgrep
    dhall-vim               # Syntax highlighting for Dhall lang
    fzf-vim                 # fuzzy finder
    ghcid                   # ghcid for Haskell
    lightline-vim           # configurable status line (can be used by coc)
    multiple-cursors        # Multiple cursors selection, etc
    neomake                 # run programs asynchronously and highlight errors
    nerdtree                # tree explorer
    nerdcommenter           # code commenter
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
    vim-hoogle              # Hoogle search (Haskell) in Vim
    vim-nix                 # nix support (highlighting, etc)
    vim-repeat              # repeat plugin commands with (.)
    vim-ripgrep             # blazing fast search using ripgrep
    vim-scala               # scala plugin
    vim-surround            # quickly edit surroundings (brackets, html tags, etc)
    vim-tmux                # syntax highlighting for tmux conf file and more
  ];

  baseConfig    = builtins.readFile ./config.vim;
  cocConfig     = builtins.readFile ./coc.vim;
  pluginsConfig = builtins.readFile ./plugins.vim;
  vimConfig     = baseConfig + pluginsConfig + cocConfig;

in

{
  programs.neovim = {
    enable       = true;
    extraConfig  = vimConfig;
    plugins      = vimPlugins;
    viAlias      = true;
    vimAlias     = true;
    vimdiffAlias = true;
    withNodeJs   = true; # for coc.nvim
    withPython   = true; # for plugins
    withPython3  = true; # for plugins
  };

  xdg.configFile."nvim/coc-settings.json".source = ./coc-settings.json;

}
