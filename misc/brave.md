## Enable Autoplay

To get audio working properly, disable shields for Hangouts and Meet and then go to:

```
chrome://settings/content/autoplay
```

And enter the following addresses:

```
https://hangouts.google.com:443
https://meet.google.com:443
```

More info: https://community.brave.com/t/no-audio-with-google-hangouts-or-hangouts-meet/93298/7

## Override keybindings

Currently there's no way to disable the annoying `Ctrl` + `Shift` + `W` (or `Q`) that closes the browser accidentally. A workaround is to set the same keybinding to open an extension instead:

```
brave://extensions/shortcuts
```

For example, set `Ctrl` + `Shift` + `W` to focus on the Google Translate extension.
