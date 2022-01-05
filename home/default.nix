{ pkgs }:

let
  hm_url = pkgs.lib.fileContents ../pinned/home-manager;

  home-manager = builtins.fetchTarball {
    name   = "home-manager-2022-01-05";
    url    = hm_url;
    sha256 = "0nbvvxyix07679sz6713civqqfvnxrnrcrbjjkqn96p3yna5a3mf";
  };

  evalHome = import "${toString home-manager}/modules";
in
{
  home-config = pkgs.lib.recurseIntoAttrs (
    evalHome {
      configuration = ./home.nix;
      lib = pkgs.lib;
      pkgs = pkgs;
    }
  );

  # Allow nix to recurse into this attribute set to look for derivations
  recurseForDerivations = true;
}
