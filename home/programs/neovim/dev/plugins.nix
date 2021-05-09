{ buildVimPluginFrom2Nix, fetchFromGitHub }:

{
  nvim-metals = buildVimPluginFrom2Nix {
    name = "nvim-metals";
    src  = fetchFromGitHub {
      owner  = "scalameta";
      repo   = "nvim-metals";
      rev    = "69a5cf9380defde5be675bd5450e087d59314855";
      sha256 = "1kjr7kgwvg1c4gglipmasvpyrk4gar4yi9kd8xdfqyka9557vyy9";
    };
  };
}
