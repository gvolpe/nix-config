{ font0 ? 16, font1 ? 18, font2 ? 40, font3 ? 28, font4 ? 7 }:

let
  bar = ''
    [bar/default]
    inherit = bar/main

    ; Text Fonts
    font-0 = Iosevka Nerd Font:style=Medium:size=${toString font0};3
    ; Icons Fonts
    font-1 = icomoon\-feather:style=Medium:size=${toString font1};3
    ; Powerline Glyphs
    font-2 = Iosevka Nerd Font:style=Medium:size=${toString font2};3
    ; Larger font size for bar fill icons
    font-3 = Iosevka Nerd Font:style=Medium:size=${toString font3};3
    ; Smaller font size for shorter spaces
    font-4 = Iosevka Nerd Font:style=Medium:size=${toString font4};3
  '';

  top = ''
    [bar/top]
    inherit = bar/default

    tray-position = center
    modules-left = right-end-top xmonad left-end-bottom right-end-top left-end-top
    modules-right = left-end-top keyboard clickable-github temperature clickable-date battery
    enable-ipc = true
  '';

  bottom = ''
    [bar/bottom]
    inherit = bar/default
    bottom = true

    tray-position = none
    modules-left = right-end-bottom mpris left-end-top cpu memory filesystem
    modules-right = left-end-bottom wired-network wireless-network pulseaudio left-end-bottom powermenu
    enable-ipc = true
  '';
in
bar + top + bottom
