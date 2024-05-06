{ ... }:

{
  programs.zathura = {
    enable = true;
    options = {
      default-bg = "#000000";
      default-fg = "#FFFFFF";
    };
    # config defaults: https://git.pwmt.org/pwmt/zathura/-/blob/develop/zathura/config.c
    mappings = {
      D = "toggle_page_mode";
    };
  };
}
