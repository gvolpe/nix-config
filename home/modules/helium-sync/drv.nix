{ coreutils
, git
, hostname
, jq
, procps
, rsync
, settings
, writeShellApplication
}:

let
  enableGitOps = if settings.sync.enableGitOps then "1" else "0";
in
writeShellApplication {
  name = "helium-sync";

  runtimeInputs = [
    coreutils
    git
    hostname
    jq
    procps
    rsync
  ];

  text = ''
    set -euo pipefail

    profile_name="${settings.profile.name}"
    profile_id="${settings.profile.id}"
    helium_config_dir="${settings.configDir}"
    sync_root="${settings.sync.directory}"
    git_ops_enabled="${enableGitOps}"

    profile_dir="$helium_config_dir/$profile_id"
    state_dir="$sync_root/$profile_name"
    snapshot_dir="$state_dir/profile/$profile_id"
    host="$(${hostname}/bin/hostname)"

    profile_rsync_args=(
      --archive
      --delete
      --prune-empty-dirs
      --include=/Bookmarks
      --include=/Bookmarks.bak
      --include=/History
      --include=/History-journal
      --include=/Favicons
      --include=/Favicons-journal
      --include=/Preferences
      --include="/Secure Preferences"
      --include="/Current Session"
      --include="/Current Tabs"
      --include="/Last Session"
      --include="/Last Tabs"
      --include="/Sessions/***"
      --include="/Sync Data/"
      --include="/Sync Data/LevelDB/"
      --include="/Sync Data/LevelDB/***"
      --exclude="*"
    )

    usage() {
      cat <<EOF
    Usage: helium-sync <command>

    Commands:
      push           Snapshot the gvolpe Helium profile; refuses while Helium is running
      pull           Restore the last profile snapshot; refuses while Helium is running
      status         Show local paths and latest synced state
      paths          Print the profile and sync paths
    EOF
    }

    note() {
      printf '%s\n' "$*" >&2
    }

    die() {
      note "helium-sync: 🚫 $*"
      exit 1
    }

    now() {
      date -u '+%Y-%m-%dT%H:%M:%SZ'
    }

    require_profile() {
      [[ -d "$profile_dir" ]] || die "profile directory not found: $profile_dir"
    }

    require_snapshot() {
      [[ -d "$snapshot_dir" ]] || die "profile snapshot not found: $snapshot_dir"
    }

    ensure_state_dir() {
      mkdir -p "$state_dir/backups" "$state_dir/profile"
    }

    helium_is_running() {
      pgrep -u "$(id -u)" -f 'net\.imput\.helium|/libexec/helium/helium|(^|[ /])helium([[:space:]]|$)' >/dev/null
    }

    require_helium_closed() {
      if helium_is_running; then
        die "Helium appears to be running; close it before syncing profile files"
      fi
    }

    maybe_git_pull() {
      if [[ -d "$sync_root/.git" && "$git_ops_enabled" == "1" ]]; then
        git -C "$sync_root" pull --ff-only
      fi
    }

    maybe_git_push() {
      if [[ -d "$sync_root/.git" && "$git_ops_enabled" == "1" ]]; then
        git -C "$sync_root" add "$profile_name"
        if ! git -C "$sync_root" diff --cached --quiet; then
          git -C "$sync_root" commit -m "helium profile sync from $host at $(now)"
        fi
        git -C "$sync_root" push
      fi
    }

    rsync_profile_state() {
      local src dest
      src="$1"
      dest="$2"

      mkdir -p "$dest"
      rsync "''${profile_rsync_args[@]}" "$src/" "$dest/"
    }

    prune_session_kind() {
      local dir pattern latest file saw_multiple
      local files=()
      dir="$1"
      pattern="$2"
      latest=""
      saw_multiple=0

      [[ -d "$dir" ]] || return 0

      shopt -s nullglob
      # shellcheck disable=SC2206
      files=("$dir"/$pattern)
      shopt -u nullglob

      for file in "''${files[@]}"; do
        [[ -n "$latest" ]] && saw_multiple=1
        latest="$file"
      done

      [[ "$saw_multiple" == "1" ]] || return 0

      for file in "''${files[@]}"; do
        [[ "$file" == "$latest" ]] || rm -f -- "$file"
      done
    }

    prune_session_rollover() {
      local base
      base="$1"

      prune_session_kind "$base/Sessions" "Session_*"
      prune_session_kind "$base/Sessions" "Tabs_*"
    }

    write_meta() {
      jq -n \
        --arg host "$host" \
        --arg ts "$(now)" \
        --arg profile "$profile_name" \
        --arg profileId "$profile_id" \
        '{
          version: 2,
          mode: "closed-profile-files",
          profile: $profile,
          profileId: $profileId,
          sourceHost: $host,
          capturedAt: $ts
        }' \
        > "$state_dir/meta.json"
    }

    push_state() {
      require_profile
      require_helium_closed
      maybe_git_pull
      ensure_state_dir
      rsync_profile_state "$profile_dir" "$snapshot_dir"
      prune_session_rollover "$snapshot_dir"
      write_meta
      maybe_git_push
      note "saved profile snapshot 💾 ✅"
      note "pushed profile state to $state_dir ✅"
    }

    backup_current_profile() {
      local backup_dir

      if [[ ! -d "$profile_dir" ]]; then
        return 0
      fi

      backup_dir="$helium_config_dir/helium-sync-backups/$(date -u '+%Y%m%d-%H%M%S')/$profile_id"
      rsync_profile_state "$profile_dir" "$backup_dir"
      note "backed up current profile state to $backup_dir ✅"
    }

    pull_state() {
      require_helium_closed
      maybe_git_pull
      require_snapshot
      backup_current_profile
      mkdir -p "$profile_dir"
      rsync_profile_state "$snapshot_dir" "$profile_dir"
      prune_session_rollover "$profile_dir"
      note "restored profile snapshot from $snapshot_dir ✅"
    }

    show_paths() {
      cat <<EOF
    profile:   $profile_dir
    sync root: $sync_root
    state:     $state_dir
    snapshot:  $snapshot_dir
    EOF
    }

    sessions_saved() {
      local session_files

      [[ -f "$snapshot_dir/Current Session" || -f "$snapshot_dir/Current Tabs" ]] && return 0
      [[ -d "$snapshot_dir/Sessions" ]] || return 1

      shopt -s nullglob
      session_files=("$snapshot_dir/Sessions"/*)
      shopt -u nullglob

      [[ "''${#session_files[@]}" -gt 0 ]]
    }

    show_status() {
      show_paths
      printf '\n'

      if [[ -d "$snapshot_dir" && -f "$state_dir/meta.json" ]] &&
        jq -e '.mode == "closed-profile-files"' "$state_dir/meta.json" >/dev/null 2>&1; then
        jq -r '"last profile push: \(.capturedAt) from \(.sourceHost) ✅"' "$state_dir/meta.json"
      elif [[ -f "$state_dir/meta.json" ]]; then
        echo "last profile push: none 🚫"
        echo "legacy live-tabs state: present 🚫"
      else
        echo "last profile push: none 🚫"
      fi

      [[ -d "$snapshot_dir" ]] && echo "profile snapshot: saved 💾" || echo "profile snapshot: none 🚫"
      [[ -f "$snapshot_dir/Bookmarks" ]] && echo "bookmarks: saved 💾" || echo "bookmarks: none 🚫"
      [[ -f "$snapshot_dir/History" ]] && echo "history: saved 💾" || echo "history: none 🚫"
      [[ -d "$snapshot_dir/Sync Data/LevelDB" ]] && echo "sync data: saved 💾" || echo "sync data: none 🚫"

      if sessions_saved; then
        echo "sessions: saved 💾"
      else
        echo "sessions: none 🚫"
      fi

      if helium_is_running; then
        echo "helium: running 🚫"
      else
        echo "helium: closed ✅"
      fi
    }

    case "''${1:-}" in
      push)
        push_state
        ;;
      pull)
        shift
        if [[ "$#" -ne 0 ]]; then
          die "pull does not accept options"
        fi
        pull_state
        ;;
      status)
        show_status
        ;;
      paths)
        show_paths
        ;;
      help|-h|--help|"")
        usage
        ;;
      *)
        usage >&2
        exit 1
        ;;
    esac
  '';
}
