{ config, lib, pkgs, ... }:

let
  metals = pkgs.metalsBuilder {
    version = "0.11.8+78-f674cb97-SNAPSHOT";
    outputHash = "sha256-aYRUgS6BSxi6yGGmOIf4cOu+1VWNEPmzB8rQu4rT92o=";
  };
in
{
  programs.neovim-ide = {
    enable = true;
    settings = {
      vim.viAlias = false;
      vim.vimAlias = true;
      vim.customPlugins = with pkgs.vimPlugins; [
        multiple-cursors
        vim-repeat
        vim-surround
      ];
      vim.lsp = {
        enable = true;
        formatOnSave = true;
        lightbulb.enable = true;
        lspsaga.enable = false;
        nvimCodeActionMenu.enable = true;
        trouble.enable = true;
        lspSignature.enable = true;
        scala = {
          inherit metals;
          enable = true;
        };
        rust.enable = false;
        nix = true;
        dhall = true;
        elm = true;
        haskell = true;
        sql = true;
        python = false;
        clang = false;
        ts = false;
        go = false;
      };
      vim.visuals = {
        enable = true;
        nvimWebDevicons.enable = true;
        lspkind.enable = true;
        indentBlankline = {
          enable = true;
          fillChar = "";
          eolChar = "";
          showCurrContext = true;
        };
        cursorWordline = {
          enable = true;
          lineTimeout = 0;
        };
      };
      vim.statusline.lualine = {
        enable = true;
        theme = "onedark";
      };
      vim.theme = {
        enable = true;
        name = "onedark";
        style = "deep";
        transparency = true;
      };
      vim.autopairs.enable = true;
      vim.autocomplete = {
        enable = true;
        type = "nvim-cmp";
      };
      vim.filetree.nvimTreeLua = {
        enable = true;
        hideDotFiles = false;
        hideFiles = [ "node_modules" ".cache" ];
      };
      vim.hop.enable = true;
      vim.tabline.nvimBufferline.enable = true;
      vim.treesitter = {
        enable = true;
        autotagHtml = true;
        context.enable = true;
      };
      vim.scala = {
        highlightMode = "treesitter";
      };
      vim.keys = {
        enable = true;
        whichKey.enable = true;
      };
      vim.comments = {
        enable = true;
        type = "nerdcommenter";
      };
      vim.shortcuts = {
        enable = true;
      };
      vim.telescope = {
        enable = true;
      };
      vim.markdown = {
        enable = true;
        glow.enable = true;
      };
      vim.git = {
        enable = true;
        gitsigns.enable = true;
      };
    };
  };
}
