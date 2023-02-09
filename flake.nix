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

    nix-filter.url = github:numtide/nix-filter;

    # Miscelaneous

    cowsay = {
      url = github:snowfallorg/cowsay;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nix-filter, ... }:
    let
      system = "x86_64-linux";

      src = nix-filter.lib.filter {
        root = ./.;

        include = [
          "home"
          "lib"
          "outputs"
          "system"
          ./flake.lock
        ];

        exclude = [
          ".git"
          ".git-crypt"
          ".github"
          "imgs"
          "notes"
          ./.gitattributes
          ./.gitignore
          ./.mergify.yml
          ./garnix.yaml
          ./flake.nix
          ./build
          ./switch
          ./GNOME.md
          ./LICENSE
          ./README.md
        ];
      };

      ci = import ./outputs/ci.nix { inherit inputs system; };

      inherit (inputs.nixpkgs.lib) mapAttrs;
    in
    rec {
      homeConfigurations =
        import ./outputs/home-conf.nix { inherit inputs system; };

      nixosConfigurations =
        import ./outputs/nixos-conf.nix { inherit inputs system; };

      packages.${system} = {
        metals = ci.metals.overrideAttrs (_: { inherit src; });
        metals-updater = ci.metals-updater.src.overrideAttrs (_: { inherit src; });
      };

      checks.${system} =
        let
          os = mapAttrs (_: c: c.config.system.build.toplevel) nixosConfigurations;
          hm = mapAttrs (_: c: c.activationPackage) homeConfigurations;
        in
        mapAttrs (_: c: c.overrideAttrs (_: { inherit src; })) (os // hm);
    };
}
