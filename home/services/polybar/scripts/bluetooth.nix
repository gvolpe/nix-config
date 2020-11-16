{ pkgs, ...}:

let
  bctl  = "/run/current-system/sw/bin/bluetoothctl";
  sctl  = "/run/current-system/sw/bin/systemctl";
in
  pkgs.writeShellScriptBin "bluetooth-ctl" ''
    bluetooth_print() {
      ${bctl} | while read -r; do
        if [ "$(${sctl} is-active "bluetooth.service")" = "active" ]; then
          printf '#1'

          devices_paired=$(${bctl} paired-devices | grep Device | cut -d ' ' -f 2)
          counter=0

          echo "$devices_paired" | while read -r line; do
            device_info=$(${bctl} info "$line")

            if echo "$device_info" | grep -q "Connected: yes"; then
              device_alias=$(echo "$device_info" | grep "Alias" | cut -d ' ' -f 2-)

              if [ $counter -gt 0 ]; then
                printf ", %s" "$device_alias"
              else
                printf " %s" "$device_alias"
              fi

              counter=$((counter + 1))
            fi
          done

          printf '\n'
        else
          echo "#2"
        fi
      done
    }

    bluetooth_toggle() {
      if ${bctl} show | grep -q "Powered: no"; then
        ${bctl} power on >> /dev/null
        ${pkgs.coreutils}/bin/sleep 1

        devices_paired=$(${bctl} paired-devices | grep Device | cut -d ' ' -f 2)
        echo "$devices_paired" | while read -r line; do
          ${bctl} connect "$line" >> /dev/null
        done
      else
        devices_paired=$(${bctl} paired-devices | grep Device | cut -d ' ' -f 2)
        echo "$devices_paired" | while read -r line; do
          ${bctl} disconnect "$line" >> /dev/null
        done

        ${bctl} power off >> /dev/null
      fi
    }

    case "$1" in
      --toggle)
        bluetooth_toggle
        ;;
      *)
        bluetooth_print
        ;;
    esac
  ''
