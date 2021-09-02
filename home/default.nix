{ pkgs }:

let
  hm_url = pkgs.lib.fileContents ../pinned/home-manager;

  home-manager = builtins.fetchTarball {
    name   = "home-manager-2021-08-21";
    url    = hm_url;
    sha256 = "0s8nlgrf16bz2bhnk0xrzvivagq55cifxf2p545c7n4zj9ryfkkp";
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
