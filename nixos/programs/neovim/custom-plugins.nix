{ config, pkgs, ... }:

{
  # blazing fast search using ripgrep
  vim-ripgrep = pkgs.vimUtils.buildVimPlugin {
    name = "vim-ripgrep";
    src = builtins.fetchTarball {
      name   = "Ripgrep v1.0.2";
      url    = "https://github.com/jremmen/vim-ripgrep/archive/v1.0.2.tar.gz";
      sha256 = "1by56rflr0bmnjvcvaa9r228zyrmxwfkzkclxvdfscm7l7n7jnmh";
    };
  };
}
