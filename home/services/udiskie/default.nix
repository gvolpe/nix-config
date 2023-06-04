{ pkgs, ... }:

{
  services.udiskie = {
    enable = true;
    automount = true;
    notify = true;
    settings = {
      program_options = {
        file_manager = "${pkgs.mimeo}/bin/mimeo";
      };
    };
    tray = "always";
  };
}
