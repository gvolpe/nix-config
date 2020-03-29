_files() {
    files=$(ls)
    COMPREPLY=( $(compgen -W "$files" -- $cur) )
    return 0
}

_projects() {
    projects=$(bloop autocomplete --mode=projects --format=bash 2> /dev/null)
    COMPREPLY=( $(compgen -W "$projects" -- $cur) )
    return 0
}

_projects_or_flags() {
    projects=$(bloop autocomplete --mode=projects --format=bash 2> /dev/null)
    if [ "$projects" == " " ] || [ "$projects" == "" ]; then
        command=$1
        flags=$(bloop autocomplete --mode=flags --format=bash --command=$command)
        COMPREPLY=( $(compgen -W "$flags" -- $cur) )
        return 0
    fi
    _projects
}

_commands() {
    commands=$(bloop autocomplete --mode=commands --format=bash 2> /dev/null || _notStarted)
    COMPREPLY=( $(compgen -W "$commands" -- $cur) )
    return 0
}

_reporters() {
    reporters=$(bloop autocomplete --mode=reporters --format=bash)
    COMPREPLY=( $(compgen -W "$reporters" -- $cur) )
    return 0
}

_protocols() {
    protocols=$(bloop autocomplete --mode=protocols --format=bash)
    COMPREPLY=( $(compgen -W "$protocols" -- $cur) )
    return 0
}

_testsfqcn() {
    project="${COMP_WORDS[2]}"
    mains=$(bloop autocomplete --mode=testsfqcn --project=$project --format=bash)
    COMPREPLY=( $(compgen -W "$mains" -- $cur) )
    return 0
}

_mainsfqcn() {
    project="${COMP_WORDS[2]}"
    mains=$(bloop autocomplete --mode=mainsfqcn --project=$project --format=bash)
    COMPREPLY=( $(compgen -W "$mains" -- $cur) )
    return 0
}

_getFlagsForCommand() {
    command=$1
    bloop autocomplete --mode=flags --format=bash --command=$command
}

_callCompletionForCommandAndFlag() {
    command="$1"
    flag="$2"

    flags=$(_getFlagsForCommand $command)
    completionFn=$(echo "$flags" | grep -- "$flag " | cut -sd' ' -f2)

    if [ "$completionFn" != "" ]; then
        $completionFn
    fi

    return 0

}

_notStarted() {
    echo "server"
}

_bloop() {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "${COMP_CWORD}" in
        1)
            _commands
            ;;

        2)
            _projects_or_flags "$prev"
            ;;

        *)
            command=${COMP_WORDS[1]}

            case "${prev}" in
                --only|-o)
                if [ "$command" == "test" ]; then
                    _testsfqcn
                fi
                ;;
                -*)
                flag="$prev"
                _callCompletionForCommandAndFlag "$command" "$flag"
                ;;

                *)
                flagNames=""
                flags=$(_getFlagsForCommand $command)

                saveIFS=$IFS
                IFS=$'\n'
                for flag in $flags; do
                    name=$(echo "$flag" | cut -d' ' -f1)
                    flagNames="$flagNames $name"
                done
                IFS=$saveIFS

                COMPREPLY=( $(compgen -W "$flagNames" -- $cur) )
                return 0
                ;;
            esac
            ;;
    esac
}

complete -F _bloop bloop
