self: super: {}

#{
  #nerdtree = super.vimPlugins.nerdtree.overrideAttrs (
    #old: {
      #src = builtins.fetchTarball {
        #name   = "NERDTree-v6.9.9";
        #url    = "https://github.com/preservim/nerdtree/archive/6.9.9.tar.gz";
        #sha256 = "0bccr18nr42vwbb4i765yxjw18piyiyd12sm4snbkkxahp9yswf7";
      #};
    #}
  #);

  #nerdtree-git-plugin = super.vimPlugins.nerdtree-git-plugin.overrideAttrs (
    #old: {
      #src = builtins.fetchTarball {
        #name   = "NERDTree-Git-Plugin-2020-09-11";
        #url    = "https://github.com/Xuyuanp/nerdtree-git-plugin/archive/a8c031f11dd312f53357729ca47ad493e798aa86.tar.gz";
        #sha256 = "1d64cmywhj43q9fkrh0kcfsxa7ijxcb1fbz38pxaacg082y6l0jy";
      #};
    #}
  #);
#}
