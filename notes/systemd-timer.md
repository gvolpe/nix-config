# systemd user timer

How to run a script every 5 minutes.

```nix
{
  systemd.user = {
    services.hello-world = {
      Unit = {
        Description = "Hello script every 5 mins";
      };

      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.hello}/bin/hello";
      };
    };

    timers.hello-world = {
      Unit = {
        Description = "Hello script every 5 mins";
      };

      Timer = {
        OnCalendar = "*:0/5";
        Unit = "hello-world.service";
      };

      Install = { WantedBy = [ "timers.target" ]; };
    };
  };
}
```
