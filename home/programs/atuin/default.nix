{ ... }:

{
  programs.atuin = {
    enable = true;
    enableFishIntegration = true;
    daemon.enable = true;
    settings = {
      auto_sync = true;
      enter_accept = false;
      filter_mode = "global";
      sync_frequency = "5m";
      sync_address = "https://atuin.gvolpe.com";
      search_mode = "prefix";
      style = "compact";
    };
  };
}
