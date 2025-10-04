#!/usr/bin/env python3
# Adapted from Andrew Song's impl: https://github.com/YaLTeR/niri/issues/426#issuecomment-3367714198
import subprocess
import json
import sys

FULLSCREEN_SIZES = [(1920, 1080), (2560, 1440)]

# tracks current and last column/row of all windows (window_id -> {current_pos: (col, row), last_pos: (col, row)})
window_positions = {}
# dict that tracks fullscreen windows and their restore positions
fullscreen_windows = {}

def is_fullscreen(window_size):
    return tuple(window_size) in FULLSCREEN_SIZES

def main():
    proc = subprocess.Popen(
        ["stdbuf", "-oL", "niri", "msg", "--json", "event-stream"],
        stdout=subprocess.PIPE,
        text=True,
    )

    for line in proc.stdout:
        line = line.strip()
        if not line:
            continue

        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            print("Failed to parse JSON")
            continue

        # initial window positions
        if "WindowsChanged" in event and not window_positions:
            windows = event["WindowsChanged"]["windows"]

            for window in windows:
                window_id = window["id"]
                layout = window.get("layout", {})
                pos = layout.get("pos_in_scrolling_layout")
                if pos is None:
                    continue  # skip floating windows
                window_positions[window_id] = {
                    "current_pos": tuple(pos),
                    "last_pos": tuple(pos),
                }

        if "WindowLayoutsChanged" not in event:
            continue

        changes = event["WindowLayoutsChanged"]["changes"]

        for change in changes:
            window_id = change[0]
            window_data = change[1]

            try:
                col, row = window_data["pos_in_scrolling_layout"]
                window_size = window_data["window_size"]
            except TypeError:
                # ignore floating windows that are made fullscreen and then go back to floating
                continue

            if window_id not in window_positions:
                window_positions[window_id] = {
                    "current_pos": (col, row),
                    "last_pos": (col, row),
                }

            # when a window is in a column with multiple rows prior to going fullscreen, there is an intermediate WindowLayoutsChanged event where the window is moved to the next column over before the fullscreen event occurs. so the true position we will need to restore to when the window exits fullscreen is actually the last_pos of the window during this intermediate event.
            if is_fullscreen(window_size):
                # if this window is not already being tracked by fullscreen windows, we know we need to save its last position first before updating so that we can restore it later
                if window_id not in fullscreen_windows:
                    prev_col, prev_row = window_positions[window_id]["last_pos"]
                    if prev_col != col:
                        # save the window id and its last position
                        fullscreen_windows[window_id] = window_positions[window_id][
                            "last_pos"
                        ]
                else:
                    # check if the window's current position has changed since entering fullscreen. if so remove it from fullscreen_windows since we dont want to restore a window that has been moved while in fullscreen as niri can handle that perfectly fine
                    if window_positions[window_id]["current_pos"] != (col, row):
                        del fullscreen_windows[window_id]

            else:
                # if window is still in fullscreen_windows, we know it just exited fullscreen, so move it back to its last position
                if window_id in fullscreen_windows:
                    prev_col, prev_row = window_positions[window_id]["last_pos"]
                    if prev_col != col:
                        subprocess.run(
                            ["niri", "msg", "action", "consume-or-expel-window-left"]
                        )
                        continue
                    destination_row = fullscreen_windows[window_id][1]
                    if destination_row != row:
                        for _ in range(row - destination_row):
                            subprocess.run(
                                ["niri", "msg", "action", "move-window-up"]
                            )
                        del fullscreen_windows[window_id]

            # only update last_pos when column doesn't change
            prev_col, prev_row = window_positions[window_id]["current_pos"]
            if is_fullscreen(window_size) or prev_col != col:
                # column changed (intermediate event) or fullscreen: only update current_pos
                window_positions[window_id]["current_pos"] = (col, row)
            else:
                # normal movement: update both
                window_positions[window_id] = {
                    "current_pos": (col, row),
                    "last_pos": (col, row),
                }

            sys.stdout.flush()

if __name__ == "__main__":
    main()
