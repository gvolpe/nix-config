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
    #nixpkgs.url = "nixpkgs/nixos-unstable";
    # nix doesn't need the full history, this should be the default ¯\_(ツ)_/¯
    nixpkgs.url = "git+https://github.com/NixOS/nixpkgs?shallow=1&ref=nixos-unstable";
    #nixpkgs.url = github:gvolpe/nixpkgs/branch-name;
    flake-schemas.url = github:DeterminateSystems/flake-schemas;

    # https://github.com/NixOS/nixpkgs/commit/c3160517fc6381f86776795e95c97b8ef7b5d2e4
    nixpkgs-mega.url = "nixpkgs/c3160517fc6381f86776795e95c97b8ef7b5d2e4";
    # https://github.com/NixOS/nixpkgs/issues/322970
    nixpkgs-zoom.url = "nixpkgs/24.05";
    # https://github.com/hyprwm/Hyprland/issues/9518
    nixpkgs-hyprland.url = "nixpkgs/b582bb5b0d7af253b05d58314b85ab8ec46b8d19";
    # https://github.com/NixOS/nixpkgs/pull/397036
    nixpkgs-zoom-fhs.url = github:Yarny0/nixpkgs/zoom-fhs;

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

    nix-index-database = {
      url = github:nix-community/nix-index-database;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index = {
      url = github:gvolpe/nix-index;
      inputs.nix-index-database.follows = "nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
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
  };

  outputs = inputs @ { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";

      overlays = import ./lib/overlays.nix { inherit inputs system; };

      pkgs = import nixpkgs {
        inherit overlays system;
        config.allowUnfree = true;
      };

      neovim = self.homeConfigurations.hyprland-hdmi.config.programs.neovim-ide.finalPackage;
    in
    {
      homeConfigurations = pkgs.builders.mkHome { };
      nixosConfigurations = pkgs.builders.mkNixos { };

      out = { inherit pkgs overlays; };

      schemas =
        inputs.flake-schemas.schemas //
        import ./lib/schemas.nix { inherit (inputs) flake-schemas; };

      packages.${system} = {
        inherit neovim;
        inherit (pkgs) bazecor quickemu metals metals-updater;
        # crappy software I need for $work
        inherit (pkgs) globalprotect-openconnect slack zoom-fhs;
      };
    };
}
