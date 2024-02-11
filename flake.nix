{
  description = "gvolpe's Home Manager & NixOS configurations";

  nixConfig = {
    access-tokens = "github.com=\${{ secrets.REPO_GITHUB_TOKEN }}";
  };

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    rycee-nurpkgs = {
      url = gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nurpkgs.url = github:nix-community/NUR;

    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sxm-flake = {
      url = github:gvolpe/sxm-flake;
      #url = git+file:///home/gvolpe/workspace/sxm/sxm-flake;
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

      pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = import ./lib/overlays.nix { inherit inputs system; };
      };

      extraArgs = { hidpi }: {
        inherit hidpi;
        inherit (inputs) gh-md-toc penguin-fox;
        inherit (inputs.rycee-nurpkgs.lib.${system}) buildFirefoxXpiAddon;
        addons = pkgs.nur.repos.rycee.firefox-addons;
      };
    in
    {
      homeConfigurations =
        import ./outputs/home-conf.nix { inherit inputs system pkgs extraArgs; };

      nixosConfigurations =
        import ./outputs/nixos-conf.nix { inherit inputs system pkgs extraArgs; };

      packages.${system} = {
        inherit (pkgs) bazecor metals metals-updater;
      };
    };
}
