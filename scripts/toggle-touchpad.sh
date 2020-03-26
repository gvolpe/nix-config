#!/bin/bash

id="$(xinput | grep Touchpad | awk ' {print $6} ' | awk -F '[=()]' ' {print $2} ')"

state="$(xinput list-props $id | grep 'Device Enabled' | awk '{print $4}')"

if [ $state -eq 1 ]
then
    xinput disable $id
    echo "Touchpad disabled."
else
    xinput enable $id
    echo "Touchpad enabled."
fi
