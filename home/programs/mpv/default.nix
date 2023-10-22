{ pkgs, ... }:

{
  # ff2mpv firefox extension native client
  home.file.".mozilla/native-messaging-hosts/ff2mpv.json".source =
    "${pkgs.ff2mpv}/lib/mozilla/native-messaging-hosts/ff2mpv.json";

  programs.mpv = {
    enable = true;
    bindings = {
      "BS" = "cycle pause";
      "SPACE" = "cycle pause";

      "\\" = "set speed 1.0";

      "UP" = "add volume 2";
      "DOWN" = "add volume -2";

      "PGUP" = "add chapter -1";
      "PGDWN" = "add chapter 1";

      "MOUSE_BTN3" = "add volume 2";
      "MOUSE_BTN4" = "add volume -2";

      "MOUSE_BTN7" = "add chapter -1";
      "MOUSE_BTN8" = "add chapter 1";

      "Alt+RIGHT" = "add video-rotate 90";
      "Alt+LEFT" = "add video-rotate -90";

      "h" = "seek -5";
      "j" = "add volume -2";
      "k" = "add volume 2";
      "l" = "seek 5";

      "Shift+LEFT" = "seek -60";
      "Shift+RIGHT" = "seek +60";

      "Ctrl+h" = "add chapter -1";
      "Ctrl+j" = "repeatable playlist-prev";
      "Ctrl+k" = "repeatable playlist-next";
      "Ctrl+l" = "add chapter 1";

      "J" = "cycle sub";
      "L" = "ab_loop";

      "a" = "add audio-delay -0.001";
      "s" = "add audio-delay +0.001";

      "O" = "cycle osc; cycle osd-bar";
    };

    config = {
      volume = 100;
      volume-max = 200;
      force-window = "yes";
      keep-open = "no";
      osc = "no";
      osd-bar = "no";
    };

    scripts = [ pkgs.mpvScripts.mpris ];
  };
}
