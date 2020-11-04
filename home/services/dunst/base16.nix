{ config, lib, pkgs, ... }:

let
  source = fetchTarball {
    name   = "base16";
    url    = "https://github.com/atpotts/base16-nix/archive/4f192af.tar.gz";
    sha256 = "1yf59vpd1i8lb2ml7ha8v6i4mv1b0xwss8ngzw08s39j838gyx6h";
  };
in
{
  imports = [ "${source}/base16.nix" ];

  # Base16 colorschemes
  # https://github.com/atpotts/base16-nix/blob/master/schemes.json
  themes.base16 = {
    enable = true;
    scheme = "tomorrow";
    variant = "tomorrow-night";
  };
}
