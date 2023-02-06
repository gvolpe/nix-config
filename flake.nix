{
  description = "gvolpe's Home Manager & NixOS configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    nixpkgs-nautilus-gtk3.url = github:NixOS/nixpkgs?ref=37bd398;

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
      # neovim-flake pushes its binaries to the cache using its own nixpkgs version
      # if we instead use ours, we'd be rebuilding all plugins from scratch
      #inputs.nixpkgs.follows = "nixpkgs";
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
  };

  outputs = inputs:
    let
      system = "x86_64-linux";
      ci = import ./outputs/ci.nix { inherit inputs system; };
    in
    rec {
      homeConfigurations =
        import ./outputs/home-conf.nix { inherit inputs system; };

      nixosConfigurations =
        import ./outputs/nixos-conf.nix { inherit inputs system; };

      packages.${system} = {
        inherit (ci) metals metals-updater;
        # for garnix: https://github.com/garnix-io/issues/issues/24
        home-edp = homeConfigurations.gvolpe-edp.activationPackage;
        home-hdmi = homeConfigurations.gvolpe-hdmi.activationPackage;
      };
    };
}
