{ pkgs ? import <nixpkgs> {
    config = { allowUnfree = true; };
  }
}:

{
  home = pkgs.callPackage ./home { };
  system = pkgs.callPackage ./system { };
}
