{ config, pkgs, ... }:

{
  asyncrun-vim = pkgs.vimUtils.buildVimPlugin {
    name = "asyncrun-vim";
    src = builtins.fetchTarball {
      name   = "AsyncRun-Vim-v2.7.5";
      url    = "https://github.com/skywind3000/asyncrun.vim/archive/2.7.5.tar.gz";
      sha256 = "02fiqf4rcrxbcgvj02mpd78wkxsrnbi54aciwh9fv5mnz5ka249m";
    };
  };

  ctrlsf-vim = pkgs.vimUtils.buildVimPlugin {
    name = "ctrlsf-vim";
    src = builtins.fetchTarball {
      name   = "CtrlSF-Vim-v2.1.2";
      url    = "https://github.com/dyng/ctrlsf.vim/archive/v2.1.2.tar.gz";
      sha256 = "1cl0hkddx7i553lld23zmjbjcqpvyvyzh5gsd0rfpwj12xpdqpw7";
    };
  };

  vim-gtfo = pkgs.vimUtils.buildVimPlugin {
    name = "vim-gtfo";
    src = builtins.fetchTarball {
      name   = "Vim-Gtfo-v2.0.0";
      url    = "https://github.com/justinmk/vim-gtfo/archive/2.0.0.tar.gz";
      sha256 = "0zq3pjdiahpq53g27rdd5jjfrz8kddqvm1jpsdqamkd1rjvrwr1y";
    };
  };

  vim-ripgrep = pkgs.vimUtils.buildVimPlugin {
    name = "vim-ripgrep";
    src = builtins.fetchTarball {
      name   = "RipGrep-v1.0.2";
      url    = "https://github.com/jremmen/vim-ripgrep/archive/v1.0.2.tar.gz";
      sha256 = "1by56rflr0bmnjvcvaa9r228zyrmxwfkzkclxvdfscm7l7n7jnmh";
    };
  };
}
