{ bc, ffmpeg, wf-recorder, writeShellScriptBin, ... }:

let
  ffm = "${ffmpeg}/bin/ffmpeg";
  ffp = "${ffmpeg}/bin/ffprobe";

  error = command: ''
    RED=$(tput setaf 1)
    GREEN=$(tput setaf 2)
    BLUE=$(tput setaf 4)
    NORMAL=$(tput sgr0)

    echo $"$RED [ERROR]  $NORMAL - Usage: '$GREEN\$$BLUE ${command}$NORMAL'"
    exit 1
  '';

  recording = writeShellScriptBin "video-record" ''
    OUTPUT_FILE=$1

    if [[ -z $OUTPUT_FILE ]]; then
      ${error "video-record <OUTPUT>"}
    fi

    ${wf-recorder}/bin/wf-recorder -f $OUTPUT_FILE
  '';

  compression = writeShellScriptBin "video-compress" ''
    INPUT_FILE=$1
    OUTPUT_FILE=$2

    if [[ -z $INPUT_FILE || -z $OUTPUT_FILE ]]; then
      ${error "video-compress <INPUT> <OUTPUT>"}
    fi

    ${ffm} -i $INPUT_FILE -c:v libx264 -crf 23 $OUTPUT_FILE
  '';

  # trim the specified seconds from a video, e.g. 'video-trim-end in.mp4 out.mp4 7' to delete the last 7 seconds
  trimming = writeShellScriptBin "video-trim-end" ''
    INPUT_FILE=$1
    OUTPUT_FILE=$2
    TRIM_TIME=$3

    if [[ -z $INPUT_FILE || -z $OUTPUT_FILE || -z $TRIM_TIME ]]; then
      ${error "video-trim-end <INPUT> <OUTPUT> <TRIM_SECONDS>"}
    fi

    VIDEO_LENGTH=$(${ffp} -v error -show_entries format=duration -of csv=p=0 $INPUT_FILE)
    END_TIME=$(${bc}/bin/bc -l <<< "$VIDEO_LENGTH - $TRIM_TIME")

    ${ffm} -ss 00:00:00 -to $END_TIME -i $INPUT_FILE -c copy $OUTPUT_FILE
  '';

  # https://www.baeldung.com/linux/ffmpeg-extract-video-frames
  extractFrame = writeShellScriptBin "video-extract-frame" ''
    INPUT_FILE=$1
    OUTPUT_FILE=$2
    AT_TIME=$3

    if [[ -z $INPUT_FILE || -z $OUTPUT_FILE || -z $AT_TIME ]]; then
      ${error "video-extract-frame <INPUT> <OUTPUT> <AT_TIME (e.g. 00:00:32)>"}
    fi

    ${ffm} -i $INPUT_FILE -ss $AT_TIME -vframes 1 $OUTPUT_FILE
  '';
in
[ compression recording trimming extractFrame ]
