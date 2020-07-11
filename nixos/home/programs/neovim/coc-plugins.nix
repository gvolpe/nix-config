{ buildVimPluginFrom2Nix, lib, nodePackages, vimPlugins }:

let
  overrides = (
    self: super:
      {
        coc-metals = buildVimPluginFrom2Nix {
          pname = "coc-metals";
          version = nodePackages.coc-metals.version;
          src = "${nodePackages.coc-metals}/lib/node_modules/coc-metals";
        };
      }
  );

  packages = (
    self: {
      coc-metals = buildVimPluginFrom2Nix {
        pname = "coc-metals";
        version = "0.9.0";
        src = builtins.fetchTarball {
          name = "coc-metals-v0.9.0";
          url = "https://github.com/scalameta/coc-metals/archive/v0.9.0.tar.gz";
          sha256 = "0r4xs0mhdxvac81cly89jqnby14h1dmrpkdfs0chz5ji4gbsgair";
        };
        meta.homepage = "https://github.com/scalameta/coc-metals/";
      };
    }
  );
in
lib.fix' (lib.extends overrides packages)
