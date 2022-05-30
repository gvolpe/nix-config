{ pkgs, ... }:

{
  xdg.configFile."orage/oragerc".text = ''
    [PARAMETERS]
    Timezone=Europe/Warsaw
    Archive limit=0
    Archive file=/home/gvolpe/.local/share/orage/orage_archive.ics
    Orage file=/home/gvolpe/.local/share/orage/orage.ics
    Sound application=play
    Main window X=2816
    Main window Y=45
    Main window size X=954
    Main window size Y=334
    Eventlist window pos X=0
    Eventlist window pos Y=0
    Eventlist window X=500
    Eventlist window Y=350
    Eventlist extra days=0
    Eventlist only first=false
    Dayview window pos X=0
    Dayview window pos Y=0
    Dayview window X=690
    Dayview window Y=390
    Dayview week mode=true
    Show Main Window Menu=false
    Select Always Today=true
    Show borders=true
    Show heading=true
    Show day names=true
    Show weeks=true
    Show todos=false
    Show event days=1
    Show in pager=true
    Show in systray=false
    Show in taskbar=true
    Start visible=true
    Start minimized=false
    Set sticked=true
    Set ontop=false
    Own icon file=${pkgs.xfce.orage}/share/icons/hicolor/160x160/apps/org.xfce.orage.xpm
    Own icon row1 data=%a
    Own icon row2 data=%d
    Own icon row3 data=%b
    XIcal week start day=6
    Show days=false
    Foreign file count=0
    Use notify foreign alarm=false
    Priority list limit=8
    Use wakeup timer=true
    Always quit=false
    File close delay=600
  '';
}
