{ buildVimPlugin, fetchFromGitHub }:

{
  # version without Makefile to run tests (need to see how to skip them and use the proper repo)
  nvim-lsp = buildVimPlugin {
    name = "nvim-lsp";
    src  = fetchFromGitHub {
      owner  = "gvolpe";
      repo   = "nvim-lsp";
      rev    = "cdab80585058967e2e9e2b54300dd672c667da7c";
      sha256 = "0mz13fg0bnfnrmvg8kr8crzl2537gk460x8i9fafkkp1c6xmg765";
    };
  };

  #nvim-lsp = buildVimPlugin {
    #name = "nvim-lsp";
    #src  = fetchFromGitHub {
      #owner  = "neovim";
      #repo   = "nvim-lsp";
      #rev    = "3f7223659f39273c5fe4f2f2d7ee8baa827a9cfb";
      #sha256 = "104439qy1nqgzxrj8pfvd05shnpywm0qj1w68rm701hwpig6j7q6";
    #};
  #};

  nvim-metals = buildVimPlugin {
    name = "nvim-metals";
    src  = fetchFromGitHub {
      owner  = "scalameta";
      repo   = "nvim-metals";
      rev    = "11388266c2d23726bccc589c3d36a051e0434471";
      sha256 = "16khwbwv9i6233p36dmppn4fppnx6mmzdmsji6knsy5db4h08prd";
    };
  };

  completion-nvim = buildVimPlugin {
    name = "completion-nvim";
    src  = fetchFromGitHub {
      owner  = "nvim-lua";
      repo   = "completion-nvim";
      rev    = "6d7c66e76ffce6ad06d82cf1842274bddff8b829";
      sha256 = "1sajay0ki9nnx9y8f6igzmsyi72wydi9fb0xzi9qr0p0xck98k34";
    };
  };

  diagnostic-nvim = buildVimPlugin {
    name = "diagnostic-nvim";
    src  = fetchFromGitHub {
      owner  = "nvim-lua";
      repo   = "diagnostic-nvim";
      rev    = "d7734f12f2c980b08c205583b7756d735222fb9f";
      sha256 = "1fsya1midzyd46x0y69v2xi0p91yg2cm4vhw36ai99kjbha005pz";
    };
  };
}
