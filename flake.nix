{
  description = "gvolpe's Home Manager & NixOS configurations";

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://cache.garnix.io"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    #nixpkgs.url = github:gvolpe/nixpkgs/branch-name;
    flake-schemas.url = github:DeterminateSystems/flake-schemas;

    # https://github.com/NixOS/nixpkgs/commit/c3160517fc6381f86776795e95c97b8ef7b5d2e4
    nixpkgs-mega.url = "nixpkgs/c3160517fc6381f86776795e95c97b8ef7b5d2e4";
    # https://github.com/NixOS/nixpkgs/issues/322970
    nixpkgs-zoom.url = "nixpkgs/24.05";

    ## nix client with schema support: see https://github.com/NixOS/nix/pull/8892
    nix-schema = {
      url = github:DeterminateSystems/nix-src/flake-schemas;
      inputs.flake-schemas.follows = "flake-schemas";
    };

    rycee-nurpkgs = {
      url = gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nurpkgs.url = github:nix-community/NUR;

    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    neovim-flake = {
      #url = git+file:///home/gvolpe/workspace/neovim-flake;
      url = github:gvolpe/neovim-flake;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-schemas.follows = "flake-schemas";
    };

    # Hyprland

    hypr-binds-flake = {
      url = github:hyprland-community/hypr-binds;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Fish shell

    fish-bobthefish-theme = {
      url = github:gvolpe/theme-bobthefish;
      flake = false;
    };

    fish-keytool-completions = {
      url = github:ckipp01/keytool-fish-completions;
      flake = false;
    };

    # Github Markdown ToC generator

    gh-md-toc = {
      url = github:ekalinin/github-markdown-toc;
      flake = false;
    };

    # Fast nix search client
    nix-search = {
      url = github:diamondburned/nix-search;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Nix linter

    fenix = {
      url = github:nix-community/fenix;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    statix = {
      url = github:nerdypepper/statix;
      inputs.fenix.follows = "fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Miscelaneous

    cowsay = {
      url = github:snowfallorg/cowsay;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Firefox style
    penguin-fox = {
      url = github:p3nguin-kun/penguinFox;
      flake = false;
    };
  };

  outputs = inputs:
    let
      system = "x86_64-linux";

      overlays = import ./lib/overlays.nix { inherit inputs system; };

      pkgs = import inputs.nixpkgs {
        inherit overlays system;
        config.allowUnfree = true;
      };

      homeConfigurations = pkgs.mkHomeConfigurations { };
      nixosConfigurations = pkgs.mkNixosConfigurations { };

      neovim = homeConfigurations.hyprland-hdmi.config.programs.neovim-ide.finalPackage;
    in
    {
      inherit homeConfigurations nixosConfigurations;

      out = { inherit pkgs overlays; };

      schemas =
        inputs.flake-schemas.schemas //
        import ./lib/schemas.nix { inherit (inputs) flake-schemas; };

      apps.${system}."nix" = {
        type = "app";
        program = "${pkgs.nix-schema}/bin/nix-schema";
      };

      packages.${system} = {
        inherit neovim;
        inherit (pkgs) bazecor metals metals-updater;
        # crappy software I need for $work
        inherit (pkgs) globalprotect-openconnect slack zoom-us;
      };
    };
}
