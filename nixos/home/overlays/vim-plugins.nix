self: super:

{
  # last working version (lots of bugs in newer versions)
  nerdtree = super.vimPlugins.nerdtree.overrideAttrs (
    old: {
      src = builtins.fetchTarball {
        name   = "NERDTree-v6.9.3";
        url    = "https://github.com/preservim/nerdtree/archive/6.9.3.tar.gz";
        sha256 = "1y41f92l6ff3hij11kxadn1l1rl1aaszk7ra951vwk88xy1maily";
      };
    }
  );
}
