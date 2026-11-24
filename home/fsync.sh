#! /usr/bin/env bash

set -euo pipefail

THINKPAD_IP="192.168.1.37"
USER="gvolpe"
DIR_SYNC="Documents/"

# the Aorus SSH key needs to be copied over to the laptop to avoid password prompts on every run
# ssh-copy-id -i ~/.ssh/id_ed25519.pub gvolpe@192.168.1.37

echo "💾🔃 Syncing $DIR_SYNC directory from Thinkpad X1 to Aorus ➡️"
rsync -av --info=progress2 --ignore-existing \
  "${USER}@${THINKPAD_IP}:${DIR_SYNC}" \
  "${HOME}/${DIR_SYNC}"

echo "💾🔃 Syncing $DIR_SYNC directory from Aorus to Thinkpad X1 ⬅️"
rsync -av --info=progress2 --ignore-existing \
  "${HOME}/${DIR_SYNC}" \
  "${USER}@${THINKPAD_IP}:${DIR_SYNC}"
