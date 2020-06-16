{ config, pkgs, ... }:

let
  plugins = pkgs.vimPlugins // pkgs.callPackage ./custom-plugins.nix {};

  vimPlugins = with plugins; [
    coc-nvim                # LSP client + autocompletion plugin
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
    vim-hoogle              # Hoogle search (Haskell) in Vim
    vim-nix                 # nix support (highlighting, etc)
    vim-repeat              # repeat plugin commands with (.)
    vim-scala               # scala plugin
    vim-surround            # quickly edit surroundings (brackets, html tags, etc)
    vim-tmux                # syntax highlighting for tmux conf file and more

    #'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } " fuzzy finder conf
    #Valloric/MatchTagAlways'                                    # highlights html enclosing tags
    #dyng/ctrlsf-vim                                            # edit file in place after searching with ripgrep
    #skywind3000/asyncrun.vim'                                   # run async commands, show result in quickfix window
    #justinmk/vim-gtfo'                                          # go to terminal or file manager
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
    withNodeJs   = false; # true if coc.nvim is needed
    withPython   = true; # for plugins
    withPython3  = true; # for plugins
  };

}
