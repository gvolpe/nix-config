{ config, ... }:

let
  filePath = "${config.dotfiles.path}/programs/wlogout/style.css";
  mutableStyle = config.lib.file.mkOutOfStoreSymlink filePath;

  file = builtins.readFile ./style.css;
  style =
    builtins.replaceStrings
      [ "/home/gvolpe/workspace/nix-config/home/programs/wlogout/assets" ]
      [ "${./assets}" ]
      file;
in
{
  programs.wlogout = {
    enable = true;
    style = if !config.dotfiles.mutable then style else mutableStyle;
    layout = [
      {
        label = "lock";
        action = "swaylock-cmd";
        text = "Lock";
        keybind = "l";
      }
      {
        label = "logout";
        action = "niri msg action quit";
        text = "Logout";
        keybind = "e";
      }
      {
        label = "shutdown";
        action = "systemctl poweroff";
        text = "Shutdown";
        keybind = "s";
      }
      {
        label = "reboot";
        action = "systemctl reboot";
        text = "Reboot";
        keybind = "r";
      }
    ];
  };
}
