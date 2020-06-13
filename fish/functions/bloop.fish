function _projects
    bloop autocomplete --format fish --mode projects 2> /dev/null
end

function _commands
    bloop autocomplete --format fish --mode commands 2> /dev/null; or _notStarted
end

function _notStarted
    echo "server"
end

function _boolean
    string split ' ' "true false"
end

function _reporters
    bloop autocomplete --format fish --mode reporters
end

function _protocols
    bloop autocomplete --format fish --mode protocols
end

function _testsfqcn
    set -l cmd (commandline -poc)
    set -e cmd[1]
    set -l project $cmd[2]
    bloop autocomplete --format fish --mode testsfqcn --project $project
end

function _mainsfqcn
    set -l cmd (commandline -poc)
    set -e cmd[1]
    set -l project $cmd[2]
    bloop autocomplete --format fish --mode mainsfqcn --project $project
end

function __assert_args_count -a count
    set -l cmd (commandline -poc)
    set -e cmd[1]
    test (count $cmd) -eq $count
end

function __assert_args_at_least_count -a count
    set -l cmd (commandline -poc)
    set -e cmd[1]
    test (count $cmd) -ge $count
end

function __assert_prev_arg_in
    set -l tokens (commandline -poc)
    if not __assert_args_at_least_count 1
        return 0
    end
    for arg in $argv
        if string match -q -- $tokens[-1] $argv[1]
            return 1
        end
    end
    return 0
end

function _project_commands
    bloop autocomplete --format bash --mode project-commands 2> /dev/null
end

function __fish_seen_subcommand_from_project_commands
    set -l prj (string split ' ' (_project_commands))
    __fish_seen_subcommand_from $prj
end

function _flags_for_command -a cmd
    bloop autocomplete --format fish --mode flags --command $cmd
end

function _is_project_command -a cmd
    if contains $cmd in (string split ' ' (_project_commands))
        echo "2"
    else
        echo "1"
    end
end

function _bloop_cmpl_ -d 'Make a completion for a subcommand' --no-scope-shadowing -a minTokens cmd
    set -e argv[1]
    set -e argv[1]
    complete -c bloop -f -n "__assert_args_at_least_count $minTokens; and __fish_seen_subcommand_from $cmd" $argv
end


# Nothing has been provided yet. Complete with commands
complete -c bloop -f -n "__assert_args_count 0" -a '(_commands)'

# Only a project command has been provided
complete -c bloop -f -n "__assert_args_count 1; and __fish_seen_subcommand_from_project_commands" -a '(_projects)'

complete -c bloop -x -n "__fish_seen_subcommand_from test; and __assert_prev_arg_in --only -o" -a '(_testsfqcn)'

for cmd in (_commands)
    set -l minTokens (_is_project_command $cmd)
    set -l flags (_flags_for_command $cmd)
    for flag in $flags
        set -l parsed (string split '#' $flag)
        if test -n $parsed[1] -a -n $parsed[2] -a -n $parsed[3]
            _bloop_cmpl_ $minTokens $cmd -l $parsed[1] -d $parsed[2] -xa $parsed[3]
        else if test -n $parsed[1] -a -z $parsed[2] -a -n $parsed[3]
            _bloop_cmpl_ $minTokens $cmd -l $parsed[1] -xa $parsed[3]
        else if test -n $parsed[1] -a -n $parsed[2] -a -z $parsed[3]
            _bloop_cmpl_ $minTokens $cmd -l $parsed[1] -d $parsed[2]
        else if test -n $parsed[1] -a -z $parsed[2] -a -z $parsed[3]
            _bloop_cmpl_ $minTokens $cmd -l $parsed[1]
        end
    end
end
