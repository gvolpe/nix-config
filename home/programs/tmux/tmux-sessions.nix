{ pkgs, ...}:

let
  tmux = "${pkgs.tmux}/bin/tmux";
in
  pkgs.writeShellScriptBin "tmux-sessions" ''
    # Exit if already in a Tmux session
    [ "$TMUX" == "" ] || exit 0

    sessions="$(${tmux} ls -F "#S") New"

    echo "λ Pick or create a Tmux session "
    echo "-------------------------------"
    echo ""

    select opt in $sessions
    do
      case $opt in
        "New")
          read -rp "Enter new session name: " SESSION_NAME
          ${tmux} new -s "$SESSION_NAME"
          break
          ;;
        *)
          ${tmux} a -t "$opt"
          break
          ;;
      esac
    done
  ''
