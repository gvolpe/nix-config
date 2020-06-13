# Use prettier Nerd Fonts glyphs
#if [ "$theme_nerd_fonts" = "yes" ]
  #set nix_glyph        \uF313 ' '
  #set tux_glyph        \uF31A ' '
  #set haskell_glyph    \uE61F ' '
  #set scala_glyph      \uE737 ' '
  #set lambda_glyph     \uFB26 ' '
  #set vim_glyph        \uE7C5 ' '
#end

#set_color 9B4BAB
#echo -ns $haskell_glyph ' '
#set_color normal

# override theme function
function __bobthefish_prompt_nix -S -d 'Display current language environment'
    if test \( -e shell.nix \) -a \( -e .envrc \)
        __bobthefish_start_segment $color_nix
        echo -ns $nix_glyph ' nix '
    else if test \( -e build.sbt \) -o \( -e build.sc \)
        __bobthefish_start_segment $color_nix
        echo -ns $scala_glyph ' scala '
    end

    set_color normal
end
