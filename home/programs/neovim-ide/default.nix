{ config, lib, pkgs, ... }:

{
  programs.neovim-ide = {
    enable = true;
    settings = {
      vim = {
        autocomplete.enable = true;
        autopairs.enable = true;
        chatgpt = {
          inherit (config.secrets) openaiApiKey;
          enable = true;
        };
        comments = {
          enable = true;
          type = "nerdcommenter";
        };
        customPlugins = with pkgs.vimPlugins; [
          vim-mergetool
          vim-multiple-cursors
          vim-repeat
        ];
        dial.enable = true;
        filetree.nvimTreeLua = {
          enable = true;
          hideDotFiles = false;
          hideFiles = [ "node_modules" ".cache" ];
          openOnSetup = false;
        };
        fx.automaton.enable = true;
        git = {
          enable = true;
          gitsigns.enable = true;
          neogit.enable = false;
        };
        harpoon.enable = true;
        hurl.enable = true;
        jujutsu.enable = true;
        keys = {
          enable = true;
          whichKey.enable = true;
        };
        #neovim.package = pkgs.neovim-nightly;
        lsp = {
          clang = false;
          dhall = false;
          elm = true;
          enable = true;
          folds = true;
          formatOnSave = false;
          go = false;
          haskell = false;
          lightbulb.enable = true;
          lspSignature.enable = false;
          lspsaga.enable = false;
          nix = {
            enable = true;
            formatter = "${lib.exe pkgs.pedantix}";
            type = "nil";
          };
          nvimCodeActionMenu.enable = true;
          python = false;
          rust.enable = false;
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
          smithy.enable = false;
          sql = false;
          trouble.enable = true;
          ts = true;
          unison = true;
        };
        markdown = {
          enable = true;
          glow.enable = true;
          render.enable = false;
        };
        mind = {
          enable = true;
          # Documents dir is synced to the cloud
          persistence = {
            dataDir = "~/Documents/mind.nvim/data";
            statePath = "~/Documents/mind.nvim/mind.json";
          };
        };
        mini.enable = true;
        neoclip.enable = true;
        notifications.enable = true;
        plantuml.enable = true;
        preventJunkFiles = true;
        shortcuts = {
          enable = true;
        };
        snacks.enable = false;
        snippets.vsnip.enable = true;
        spider = {
          enable = false;
          skipInsignificantPunctuation = true;
        };
        statusline.lualine = {
          enable = true;
          theme = "onedark";
        };
        surround = {
          enable = true;
        };
        tabline.nvimBufferline.enable = true;
        telescope = {
          enable = true;
          mediaFiles.enable = true;
          tabs.enable = true;
        };
        theme = {
          enable = true;
          name = "onedark";
          style = "deep";
          transparency = true;
        };
        tide = {
          enable = true;
          keys.splits.vertical = "~";
        };
        todo.enable = true;
        treesitter = {
          autotagHtml = true;
          context.enable = true;
          enable = true;
          textobjects.enable = false;
        };
        viAlias = false;
        vimAlias = true;
        visuals = {
          cursorWordline = {
            enable = true;
            lineTimeout = 0;
          };
          enable = true;
          indentBlankline = {
            enable = true;
            eolChar = "";
            fillChar = "";
            showCurrContext = true;
          };
          lspkind.enable = true;
          noice.enable = true;
          nvimWebDevicons.enable = true;
        };
        zen.enable = true;
      };
    };
  };
}
