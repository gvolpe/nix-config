{ ... }:

{
  programs.dank-calendar = {
    enable = true;
    settings = {
      remindersEnabled = true;
      use24HourClock = true;
      defaultReminderMinutes = 10;
      snoozeMinutes = 5;
    };
    systemd.enable = true;
  };
}

