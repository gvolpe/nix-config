{ lib, pkgs, ... }:

let
  graphicalService = { description, execStart, restart ? "on-failure" }:
    {
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        ExecStart = execStart;
        Restart = restart;
      };
      Unit = {
        After = [ "graphical-session.target" ];
        ConditionEnvironment = "WAYLAND_DISPLAY";
        Description = description;
        PartOf = [ "graphical-session.target" ];
      };
    };
in
{
  systemd.user.services = {
    nfsm = graphicalService {
      description = "Niri fullscreen manager";
      execStart = lib.exe pkgs.nfsm;
    };
    nsticky = graphicalService {
      description = "Niri sticky windows support";
      execStart = lib.exe pkgs.nsticky;
    };
    sway-audio-idle-inhibit = graphicalService {
      description = "Audio idle inhibitor";
      execStart = "${lib.exe pkgs.sway-audio-idle-inhibit} --ignore-source-outputs 'cava,PulseAudio Volume Control'";
      restart = "always";
    };
  };
}
