{ config, pkgs, ... }:

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
        #neovim.package = pkgs.neovim-nightly;
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
            enable = true;
            metals = {
              # using snapshot for https://github.com/scalameta/metals/pull/7097
              package = pkgs.callPackage ./metals.nix { };
              # best effort compilation + vs code default settings: https://github.com/scalameta/metals-vscode/blob/1e10e1a71cf81569ea65329ec2aa0aa1cb6ad682/packages/metals-vscode/package.json#L232
              serverProperties = [
                "-Dmetals.enable-best-effort=true"
                "-Xmx2G"
                "-XX:+UseZGC"
                "-XX:ZUncommitDelay=30"
                "-XX:ZCollectionInterval=5"
                "-XX:+IgnoreUnrecognizedVMOptions"
              ];
            };
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
        hurl.enable = true;
        plantuml.enable = true;
        fx.automaton.enable = true;
        snacks.enable = true;
        visuals = {
          enable = true;
          noice.enable = true;
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
        mini.enable = true;
        neoclip.enable = true;
        dial.enable = true;
        harpoon.enable = true;
        hop.enable = true;
        notifications.enable = true;
        snippets.vsnip.enable = true;
        tide = {
          enable = true;
          keys.splits.vertical = "~";
        };
        todo.enable = true;
        tabline.nvimBufferline.enable = true;
        zen.enable = true;
        treesitter = {
          enable = true;
          autotagHtml = true;
          context.enable = true;
          textobjects.enable = false;
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
          mediaFiles.enable = true;
          tabs.enable = true;
        };
        markdown = {
          enable = true;
          glow.enable = true;
          render.enable = true;
        };
        chatgpt = {
          enable = true;
          inherit (config.secrets) openaiApiKey;
        };
        git = {
          enable = true;
          gitsigns.enable = true;
          neogit.enable = false;
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
