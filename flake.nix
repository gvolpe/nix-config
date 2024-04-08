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
    flake-schemas.url = "github:gvolpe/flake-schemas";

    ## nix client with schema support: see https://github.com/NixOS/nix/pull/8892
    nix-schema = {
      inputs.flake-schemas.follows = "flake-schemas";
      url = "github:DeterminateSystems/nix-src/flake-schemas";
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

    # Nix linter

    statix = {
      url = github:nerdypepper/statix;
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
        # workaround for: https://github.com/NixOS/nixpkgs/issues/290949
        config.permittedInsecurePackages = [
          "freeimage-unstable-2021-11-01"
        ];
      };
    in
    {
      schemas =
        inputs.flake-schemas.schemas //
        import ./lib/schemas.nix { inherit (inputs) flake-schemas; };

      homeConfigurations = pkgs.mkHomeConfigurations { };
      nixosConfigurations = pkgs.mkNixosConfigurations { };

      out = { inherit pkgs overlays; };

      apps.${system}."nix" = {
        type = "app";
        program = "${pkgs.nix-schema}/bin/nix-schema";
      };

      packages.${system} = {
        inherit (pkgs) bazecor metals metals-updater;
      };
    };
}
