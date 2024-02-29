{ ... }:

{
  accounts.calendar = {
    basePath = "kalendar";
    accounts.gvolpe = {
      primary = true;
      khal = {
        enable = true;
        color = "light green";
        glob = "*";
        priority = 10;
        readOnly = false;
        type = "calendar";
      };
      # see https://github.com/nix-community/home-manager/issues/4675#issuecomment-1967853432
      local = {
        type = "filesystem";
        fileExt = ".ics";
      };
      primaryCollection = "gvolpe"; # workaround
    };
  };

  programs.khal = {
    enable = true;
    locale = {
      # Format strings are for Python strftime, similarly to strftime(3).
      dateformat = "%x";
      datetimeformat = "%c";
      default_timezone = "CET";
      # Monday is 0, Sunday is 6
      firstweekday = 0;
      longdateformat = "%x";
      longdatetimeformat = "%c";
      timeformat = "%X";
      unicode_symbols = true;
    };
    settings = {
      default = {
        default_calendar = "gvolpe";
        timedelta = "5d";
      };
      view = {
        agenda_event_format =
          "{calendar-color}{cancelled}{start-end-time-style} {title}{repeat-symbol}{reset}";
      };
    };
  };
}
