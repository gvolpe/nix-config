{ pkgs ? import <nixpkgs> {
    config = {
      allowUnfree = true;
      nix.extraOptions = ''
        experimental-features = nix-command
      '';
    };
  }
}:

{
  home = pkgs.callPackage ./home { };
  system = pkgs.callPackage ./system { };
}
