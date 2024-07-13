{ pkgs, lib, ... }:

let
  fzfConfig = ''
    set -x FZF_DEFAULT_OPTS "--preview='bat {} --color=always'" \n
    set -x SKIM_DEFAULT_COMMAND "rg --files || fd || find ."
  '';

  themeConfig = ''
    set -g theme_display_date no
    set -g theme_display_git_master_branch no
    set -g theme_nerd_fonts yes
    set -g theme_newline_cursor yes
    set -g theme_color_scheme solarized
  '';

  custom = pkgs.callPackage ./plugins.nix { };

  fenv = {
    inherit (pkgs.fishPlugins.foreign-env) src;
    name = "foreign-env";
  };

  fishConfig = ''
    bind \t accept-autosuggestion
    set fish_greeting
  '' + fzfConfig + themeConfig;

  dc = "${pkgs.docker-compose}/bin/docker-compose";

  # set foot's terminal background color with no transparency
  # see: https://codeberg.org/dnkl/foot/src/commit/aea16ba5d2896ef22bf0bea45e5e8142c0ff1c2a/doc/foot-ctlseqs.7.scd#L669
  foot-opaque = ''printf "\033]11;[100]#000000"'';
  # reset color to default value
  # see: https://codeberg.org/dnkl/foot/src/commit/c45231ef89faeee0674e405e94ef6a92eb6726a0/doc/foot.ini.5.scd#L604
  foot-reset = ''printf "\033]11;[50]#242424"'';
in
{
  programs.fish = {
    enable = true;
    plugins = [ custom.theme fenv ];
    interactiveShellInit = ''
      eval (direnv hook fish)
      any-nix-shell fish --info-right | source
    '';
    shellAliases = {
      inherit dc foot-opaque foot-reset;
      cat = "bat";
      dps = "${dc} ps";
      dcd = "${dc} down --remove-orphans";
      drm = "docker images -a -q | xargs docker rmi -f";
      du = "${pkgs.ncdu}/bin/ncdu --color dark -rr -x";
      ls = "${pkgs.eza}/bin/eza";
      ll = "ls -a";
      ns = "nix-search";
      ".." = "cd ..";
      firefox = "${lib.exe pkgs.firefox-beta-bin}";
      ping = "${pkgs.prettyping}/bin/prettyping";
      tree = "${pkgs.eza}/bin/eza -T";
      xdg-open = "${pkgs.mimeo}/bin/mimeo";
    };
    shellInit = fishConfig;
  };

  xdg.configFile."fish/completions/keytool.fish".text = custom.completions.keytool;
  xdg.configFile."fish/functions/fish_prompt.fish".text = custom.prompt;
}
