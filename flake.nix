{
  description = "gvolpe's Home Manager & NixOS configurations";

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://gvolpe-nixos.cachix.org"
      "https://helium-nix.cachix.org"
      "https://install.determinate.systems"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "gvolpe-nixos.cachix.org-1:0MPlBIMwYmrNqoEaYTox15Ds2t1+3R+6Ycj0hZWMcL0="
      "helium-nix.cachix.org-1:a8YPjt9O4GPyX0u3gjg/aWpb14teU9aRiSG/MOaSFgw="
      "cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="
    ];
  };

  inputs = {
    #nixpkgs.url = "nixpkgs/nixos-unstable";
    #nixpkgs.url = github:gvolpe/nixpkgs/branch-name;
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.zst";
    determinate-nix = {
      url = "https://flakehub.com/f/DeterminateSystems/nix-src/*";
      inputs.flake-parts.follows = "flake-parts";
    };
    flake-schemas.url = "https://flakehub.com/f/DeterminateSystems/flake-schemas/0";

    # inputs to avoid different flakes bringing their own
    systems.url = github:nix-systems/x86_64-linux;
    flake-parts.url = github:hercules-ci/flake-parts;
    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.systems.follows = "systems";
    };

    # https://github.com/hyprwm/Hyprland/issues/9518
    nixpkgs-hyprland.url = "nixpkgs/b582bb5b0d7af253b05d58314b85ab8ec46b8d19";

    rycee-nurpkgs = {
      url = gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nurpkgs = {
      url = github:nix-community/NUR;
      inputs.flake-parts.follows = "flake-parts";
    };

    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dots.url = github:gvolpe/dots;
    #dots.url = git+file:///home/gvolpe/workspace/dots;

    neovim-flake = {
      #url = git+file:///home/gvolpe/workspace/neovim-flake;
      url = github:gvolpe/neovim-flake;
      inputs.flake-schemas.follows = "flake-schemas";
      inputs.flake-utils.follows = "flake-utils";
      inputs.neovim-nightly-overlay.inputs.flake-parts.follows = "flake-parts";
      inputs.nixd.inputs.flake-parts.follows = "flake-parts";
    };

    nix-index-database = {
      url = github:nix-community/nix-index-database;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index = {
      #url = git+file:///home/gvolpe/workspace/nix-index;
      url = github:gvolpe/nix-index;
      inputs.nix-index-database.follows = "nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Secrets
    agenix = {
      url = github:ryantm/agenix;
      inputs.home-manager.follows = "home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "systems";
    };

    # Hyprland
    hyprland = {
      url = github:hyprwm/Hyprland?ref=v0.46.2;
      flake = false;
    };

    hypr-binds-flake = {
      url = github:hyprland-community/hypr-binds;
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Niri
    niri = {
      url = github:niri-wm/niri;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nfsm-flake = {
      url = github:gvolpe/nfsm;
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "systems";
    };

    niri-scratchpad-flake = {
      #url = git+file:///home/gvolpe/workspace/niri-scratchpad;
      url = github:gvolpe/niri-scratchpad;
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "systems";
    };

    nsticky-flake = {
      url = github:lonerOrz/nsticky;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nirimation = {
      url = github:Xansidev/nirimation;
      flake = false;
    };

    niri-shaders = {
      url = github:liixini/shaders;
      flake = false;
    };

    sunix = {
      #url = git+file:///home/gvolpe/workspace/sunix;
      url = github:gvolpe/sunix;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    waycal = {
      url = github:forrestknight/waycal;
      flake = false;
    };

    wooz-flake = {
      url = github:negrel/wooz;
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wshowkeys = {
      url = github:DreamMaoMao/wshowkeys;
      flake = false;
    };

    # Fish shell

    fish-bobthefish-theme = {
      #url = git+file:///home/gvolpe/workspace/theme-bobthefish;
      url = github:gvolpe/theme-bobthefish;
      flake = false;
    };

    fish-keytool-completions = {
      url = github:ckipp01/keytool-fish-completions;
      flake = false;
    };

    # Helium browser
    helium-nix.url = github:penal-colony/helium-nix?rev=62fc5f5f35de7cf6eafce21ef76e23d12965521f;

    # Github Markdown ToC generator
    gh-md-toc = {
      url = github:ekalinin/github-markdown-toc;
      flake = false;
    };

    # Fast nix search client
    nix-search = {
      url = github:diamondburned/nix-search;
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Gram editor
    gram-extensions = {
      url = "git+https://tangled.org/niklaskorz.eu/nix-gram-extensions";
      # codeberg is very slow and unreliable
      #url = "git+https://codeberg.org/niklaskorz/nix-gram-extensions";
      #inputs.nixpkgs.follows = "nixpkgs"; # needs https://github.com/NixOS/nixpkgs/pull/537433
    };

    # Scala plugin for gram
    metals-zed = {
      #url = github:scalameta/metals-zed?ref=v0.2.4;
      url = github:scalameta/metals-zed;
      flake = false;
    };

    # Revision that metals-zed pins in their extension.toml file
    tree-sitter-scala = {
      url = github:tree-sitter/tree-sitter-scala?ref=97aead18d97708190a51d4f551ea9b05b60641c9;
      flake = false;
    };

    # Miscelaneous

    nix-graph = {
      url = github:AlexAntonik/nix-graph;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nord-tmux = {
      url = github:arcticicestudio/nord-tmux;
      flake = false;
    };

    pedantix = {
      url = github:swarsel/pedantix;
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    snitch = {
      url = github:karol-broda/snitch;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wrappers = {
      url = "git+https://codeberg.org/viperML/wrapper-manager";
    };
  };

  outputs = inputs @ { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";

      overlays = import ./lib/overlays.nix { inherit inputs system; };

      pkgs = import nixpkgs {
        inherit overlays system;
        config = {
          allowUnfree = true;
          contentAddressedByDefault = false;
        };
      };

      inherit (self.homeConfigurations.niri.config.wrappers) fastfetch;

      neovim = self.homeConfigurations.niri-desktop.config.programs.neovim-ide.finalPackage;
    in
    {
      lib = {
        mkHome = { extraHomeConfig ? { }, pkgs }:
          import ./outputs/hm.nix { inherit inputs pkgs extraHomeConfig; };

        mkNixos = { extraSystemConfig ? { }, pkgs }:
          import ./outputs/os.nix { inherit inputs pkgs extraSystemConfig; };
      };

      homeConfigurations = self.lib.mkHome { inherit pkgs; };
      nixosConfigurations = self.lib.mkNixos { inherit pkgs; };

      out = { inherit pkgs overlays; };

      schemas =
        inputs.flake-schemas.schemas //
        import ./lib/schemas.nix { inherit (inputs) flake-schemas; };

      packages.${system} = {
        inherit fastfetch neovim;
        inherit (inputs.determinate-nix.packages.${system}) nix;
        inherit (pkgs) bazecor metals metals-updater quickemu slack;
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          # cd home/secrets && agenix -e new-secret.age
          inputs.agenix.packages.${system}.default
        ];
      };
    };
}
