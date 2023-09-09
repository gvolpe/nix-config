{ acpi, libnotify, writeShellScriptBin }:

let
  xacpi = "${acpi}/bin/acpi";
  notify = "${libnotify}/bin/notify-send";
in
writeShellScriptBin "battery-level-check" ''
  check_battery_level() {
    PERCENT=`${xacpi} | cut -d , -f2`
    PERCENT=''${PERCENT%\%}
    STATUS=`${xacpi} -a`

    if [[ $PERCENT -lt 10 && $STATUS == *"off-line"* ]]; then
      ${notify} --urgency=critical 'Low battery!' "$PERCENT% left"
    fi
  }

  sleep_pid=0

  while true; do
    check_battery_level
    sleep 60 &
    sleep_pid=$!
    wait
  done
''
