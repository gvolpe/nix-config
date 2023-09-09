{ lib, pkgs, ... }:

let
  metals = pkgs.callPackage ./metals.nix { };

  openaiApiKey = lib.secretManager {
    filepath = ../../secrets/openai-api-key;
    fileAction = file: lib.removeNewline (builtins.readFile file);
    encryptedSha256 = "80c866e45d4344f12fa4d98d29cadd27d31d68fd402c97fe05eced6b470ebae8";
    emptyValue = "SECRET";
  };
in
{
  programs.neovim-ide = {
    enable = true;
    settings = {
      vim = {
        viAlias = false;
        vimAlias = true;
        preventJunkFiles = true;
        customPlugins = with pkgs.vimPlugins; [
          multiple-cursors
          vim-mergetool
          vim-repeat
        ];
        neovim.package = pkgs.neovim-nightly;
        lsp = {
          enable = true;
          folds = true;
          formatOnSave = false;
          lightbulb.enable = true;
          lspsaga.enable = false;
          nvimCodeActionMenu.enable = true;
          trouble.enable = true;
          lspSignature.enable = false;
          nix = {
            enable = true;
            type = "nil";
          };
          scala = {
            inherit metals;
            enable = true;
          };
          ts = true;
          smithy.enable = true;
          rust.enable = false;
          dhall = false;
          elm = true;
          haskell = false;
          sql = false;
          python = false;
          clang = false;
          go = false;
        };
        plantuml.enable = true;
        fx.automaton.enable = true;
        visuals = {
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
        statusline.lualine = {
          enable = true;
          theme = "onedark";
        };
        theme = {
          enable = true;
          name = "onedark";
          style = "deep";
          transparency = true;
        };
        autopairs.enable = true;
        autocomplete.enable = true;
        filetree.nvimTreeLua = {
          enable = true;
          hideDotFiles = false;
          hideFiles = [ "node_modules" ".cache" ];
          openOnSetup = false;
        };
        neoclip.enable = true;
        dial.enable = true;
        hop.enable = true;
        notifications.enable = true;
        todo.enable = true;
        tabline.nvimBufferline.enable = true;
        treesitter = {
          enable = true;
          autotagHtml = true;
          context.enable = true;
        };
        keys = {
          enable = true;
          whichKey.enable = true;
        };
        comments = {
          enable = true;
          type = "nerdcommenter";
        };
        shortcuts = {
          enable = true;
        };
        surround = {
          enable = true;
        };
        telescope = {
          enable = true;
        };
        markdown = {
          enable = true;
          glow.enable = true;
        };
        chatgpt = {
          enable = true;
          inherit openaiApiKey;
        };
        git = {
          enable = true;
          gitsigns.enable = true;
        };
        spider = {
          enable = false;
          skipInsignificantPunctuation = true;
        };
        mind = {
          enable = true;
          # Documents dir is synced to the cloud
          persistence = {
            dataDir = "~/Documents/mind.nvim/data";
            statePath = "~/Documents/mind.nvim/mind.json";
          };
        };
      };
    };
  };
}
