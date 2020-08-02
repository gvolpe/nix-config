self: super:

{
  nerdtree = super.vimPlugins.nerdtree.overrideAttrs (
    old: {
      src = builtins.fetchTarball {
        name   = "NERDTree-v6.9.5";
        url    = "https://github.com/preservim/nerdtree/archive/6.9.5.tar.gz";
        sha256 = "0cgib0afmm0v1lwqbkrmyw0s7z69g4qz2lcijrp7h3vd5ljzkqjl";
      };
    }
  );

  nerdtree-git-plugin = super.vimPlugins.nerdtree-git-plugin.overrideAttrs (
    old: {
      src = builtins.fetchTarball {
        name   = "NERDTree-Git-Plugin-2020-07-16";
        url    = "https://github.com/Xuyuanp/nerdtree-git-plugin/archive/d19ee8a08450bbdb50c2ab55ece4e95787a97e78.tar.gz";
        sha256 = "043gn5k99ydkhqfxjl02hyna2bmrawkhkcljfbak1nny1f185c3x";
      };
    }
  );
}
