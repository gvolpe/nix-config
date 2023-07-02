{ pkgs, ... }:

let
  # obtained via `autorandr --fingerprint`
  msiOptixId = "00ffffffffffff003669a23d010101011c1d010380462778eef8c5a556509e26115054bfef80714f81c08100814081809500b300a9404dd000a0f0703e8030203500bc862100001e000000fd001e4b1f873c000a202020202020000000fc004d4147333231435552560a2020000000ff0044413241303139323830303430012c020346f153010203040510111213141f2021225d5e5f616023091707830100006d030c001000383c20006003020167d85dc401788003e40f000006e305e301e60607015c5c0004740030f2705a80b0588a00bc862100001e565e00a0a0a0295030203500bc862100001e1b2150a051001e3048883500bc862100001e0000002f";
  tongfangId = "00ffffffffffff004d10c31400000000091d0104a522137807de50a3544c99260f5054000000010101010101010101010101010101011a3680a070381d403020350058c210000018000000fd00303c42420d010a202020202020000000100000000000000000000000000000000000fc004c513135364d314a5730310a20006b";

  notify = "${pkgs.libnotify}/bin/notify-send";
in
{
  programs.autorandr = {
    enable = true;

    hooks = {
      predetect = { };

      preswitch = { };

      postswitch = {
        "notify-xmonad" = ''
          ${notify} -i display "Display profile" "$AUTORANDR_CURRENT_PROFILE"
        '';

        "change-dpi" = ''
          case "$AUTORANDR_CURRENT_PROFILE" in
            away)
              DPI=96
              ;;
            home)
              DPI=178
              ;;
            *)
              ${notify} -i display "Unknown profle: $AUTORANDR_CURRENT_PROFILE"
              exit 1
          esac

          echo "Xft.dpi: $DPI" | ${pkgs.xorg.xrdb}/bin/xrdb -merge
        '';
      };
    };

    profiles = {
      "away" = {
        fingerprint = {
          eDP = tongfangId;
        };

        config = {
          eDP = {
            enable = true;
            crtc = 0;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
            rate = "60.04";
            rotate = "normal";
          };
        };
      };

      "home" = {
        fingerprint = {
          HDMI-A-0 = msiOptixId;
          eDP = tongfangId;
        };

        config = {
          HDMI-A-0 = {
            enable = true;
            crtc = 0;
            position = "0x0";
            mode = "3840x2160";
            rate = "30.00";
          };
          eDP = {
            enable = true;
            crtc = 1;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
            rate = "60.04";
            rotate = "normal";
          };
        };
      };
    };

  };
}
