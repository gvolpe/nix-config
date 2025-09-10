{ bc, ffmpeg, wf-recorder, writeShellScriptBin, ... }:

let
  ffm = "${ffmpeg}/bin/ffmpeg";
  ffp = "${ffmpeg}/bin/ffprobe";

  recording = writeShellScriptBin "video-record" ''
    OUTPUT_FILE=$1

    if [[ -z $OUTPUT_FILE ]]; then
      notify-send -u critical 'Error' "Missing output file, e.g. '$ video-record output.mkv'" -t 5000
      exit 1
    fi

    ${wf-recorder}/bin/wf-recorder -f $OUTPUT_FILE
  '';

  compression = writeShellScriptBin "video-compress" ''
    INPUT_FILE=$1
    OUTPUT_FILE=$2

    if [[ -z $INPUT_FILE ]]; then
      notify-send -u critical 'Error' "Missing input file, e.g. '$ video-compress in.mkv out.mkv'" -t 5000
      exit 1
    fi

    if [[ -z $OUTPUT_FILE ]]; then
      notify-send -u critical 'Error' "Missing output file, e.g. '$ video-compress in.mkv out.mkv'" -t 5000
      exit 1
    fi

    ${ffm} -i $INPUT_FILE -c:v libx264 -crf 23 $OUTPUT_FILE
  '';

  # trim the specified seconds from a video, e.g. 'video-trim-end in.mp4 out.mp4 7' to delete the last 7 seconds
  trimming = writeShellScriptBin "video-trim-end" ''
    INPUT_FILE=$1
    OUTPUT_FILE=$2
    TRIM_TIME=$3

    if [[ -z $INPUT_FILE ]]; then
      notify-send -u critical 'Error' "Missing input file, e.g. '$ video-trim-end in.mkv out.mkv 15'" -t 5000
      exit 1
    fi

    if [[ -z $OUTPUT_FILE ]]; then
      notify-send -u critical 'Error' "Missing output file, e.g. '$ video-trim-end in.mkv out.mkv 15'" -t 5000
      exit 1
    fi

    if [[ -z $TRIM_TIME ]]; then
      notify-send -u critical 'Error' "Missing trimming time, e.g. '$ video-trim-end in.mkv out.mkv 15'" -t 5000
      exit 1
    fi

    VIDEO_LENGTH=$(${ffp} -v error -show_entries format=duration -of csv=p=0 $INPUT_FILE)
    END_TIME=$(${bc}/bin/bc -l <<< "$VIDEO_LENGTH - $TRIM_TIME")

    ${ffm} -ss 00:00:00 -to $END_TIME -i $INPUT_FILE -c copy $OUTPUT_FILE
  '';
in
[ compression recording trimming ]
