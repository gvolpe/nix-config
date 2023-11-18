{ ... }:

{
  accounts.calendar.accounts.gvolpe = {
    primary = true;
    khal = {
      enable = true;
      color = "light green";
      glob = "*";
      priority = 10;
      readOnly = false;
      type = "discover";
    };
  };

  programs.khal = {
    enable = true;
    locale = { };
    # Format strings are for Python strftime, similarly to strftime(3).
    #dateformat = "%x";
    #datetimeformat = "%c";
    ##default.timezone = "UTC+1";
    ## Monday is 0, Sunday is 6
    #firstweekday = 0;
    #longdateformat = "%x";
    #longdatetimeformat = "%c";
    #timeformat = "%X";
    #unicode_symbols = true;
    #};

    #settings = {
    ##gvolpe = {
    ##default_calendar = "Calendar";
    ##timedelta = "5d";
    ##};
    #default = {
    #default_calendar = "Calendar";
    #timedelta = "5d";
    #};
    #view = {
    #agenda_event_format =
    #"{calendar-color}{cancelled}{start-end-time-style} {title}{repeat-symbol}{reset}";
    #};
    #};
  };
}
