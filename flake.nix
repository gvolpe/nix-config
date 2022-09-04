{
  description = "gvolpe's Home Manager & NixOS configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

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

    # LaTeX stuff

    tex2nix = {
      url = github:Mic92/tex2nix/4b17bc0;
      inputs.utils.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
    in
    {
      homeConfigurations = (
        import ./outputs/home-conf.nix {
          inherit inputs system;
        }
      );

      nixosConfigurations = (
        import ./outputs/nixos-conf.nix {
          inherit inputs system;
        }
      );
    };
}
