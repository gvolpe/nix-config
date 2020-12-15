# xrandr

Querying the available resolutions for the different displays is as easy as running `xrandr` without arguments.

```shell
$ xrandr
Screen 0: minimum 320 x 200, current 3840 x 2160, maximum 16384 x 16384
HDMI-A-0 connected primary 3840x2160+0+0 (normal left inverted right x axis y axis) 700mm x 390mm
   3840x2160     60.00 +  60.00    50.00    59.94    30.00*   25.00    24.00    29.97    23.98
   2560x1440     59.95
   1920x1200     60.00
   1920x1080     60.00    50.00    59.94    30.00    25.00    24.00    29.97    23.98
   1600x1200     60.00
   1680x1050     59.88
   1280x1024     75.02    60.02
   1440x900      59.90
   1280x960      60.00
   1360x768      59.80
   1280x800      59.91
   1152x864      75.00
   1280x720      60.00    50.00    59.94
   1024x768      75.03    70.07    60.00
   832x624       74.55
   800x600       72.19    75.00    60.32    56.25
   720x576       50.00
   720x480       60.00    59.94
   640x480       75.00    72.81    66.67    60.00    59.94
   720x400       70.08
eDP connected 1920x1080+0+0 (normal left inverted right x axis y axis) 344mm x 194mm
   1920x1080     60.04*+
   1680x1050     60.04
   1280x1024     60.04
   1440x900      60.04
   1280x800      60.04
   1280x720      60.04
   1024x768      60.04
   800x600       60.04
   640x480       60.04
```

To manually set the mode and refresh rate, we run.

```shell
$ xrandr --output HDMI-A-0 --mode 3840x2160 --rate 30.00
```

Though, the changes will be lost upon restart. The [official documentation](https://www.x.org/releases/current/doc/man/man5/xorg.conf.5.xhtml) shows how to change to a specific resolution and refresh rate manually but not how to do it via the `xorg.conf` configuration file so I made it work adding a new mode via `Modeline`.

To get the expected resolution we run `cvt` this way.

```shell
$ cvt 3840 2160 30.02
# 3840x2160 30.00 Hz (CVT) hsync: 66.00 kHz; pclk: 339.00 MHz
Modeline "3840x2160_30.02"  339.00  3840 4080 4488 5136  2160 2163 2168 2200 -hsync +vsync
```

I ended up using `30.02` as the expected refresh rate because it generates the correct mode.

To make the change reproducible, we need to add it to our `configuration.nix`.

```nix
services.xserver = {
  videoDrivers = [ "amdgpu" ];

  xrandrHeads = [
    { output = "HDMI-A-0";
      primary = true;
      monitorConfig = ''
        Modeline "3840x2160_30.02"  339.00  3840 4080 4488 5136  2160 2163 2168 2200 -hsync +vsync
        Option "PreferredMode" "3840x2160_30.02"
        Option "Position" "0 0"
      '';
    }
    { output = "eDP";
      monitorConfig = ''
        Option "PreferredMode" "1920x1980"
        Option "Position" "0 0"
      '';
    }
  ];

  resolutions = [
    { x = 2048; y = 1152; }
    { x = 1920; y = 1080; }
    { x = 2560; y = 1440; }
    { x = 3072; y = 1728; }
    { x = 3840; y = 2160; }
  ];
};
```
