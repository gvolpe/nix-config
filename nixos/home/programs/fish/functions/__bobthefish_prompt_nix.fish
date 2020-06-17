# override theme function
function __bobthefish_prompt_nix -S -d 'Display current language environment'
    if test \( -e shell.nix \) -a \( -e .envrc \)
        __bobthefish_start_segment $color_nix
        echo -ns $nix_glyph ' nix '
    else if test \( -e build.sbt \) -o \( -e build.sc \)
        __bobthefish_start_segment $color_nix
        set scala_glyph \uE737 ' '
        echo -ns $scala_glyph 'scala '
    end

    set_color normal
end
