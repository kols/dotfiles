function _fzf_wrapper --description "Prepares some environment variables before executing fzf."
    # Make sure fzf uses fish to execute preview commands, some of which
    # are autoloaded fish functions so don't exist in other shells.
    # Use --local so that it doesn't clobber SHELL outside of this function.
    set --local --export SHELL (command --search fish)

    # If FZF_DEFAULT_OPTS is not set, then set some sane defaults.
    # See https://github.com/junegunn/fzf#environment-variables
    if not set --query FZF_DEFAULT_OPTS
        # cycle allows jumping between the first and last results, making scrolling faster
        # layout=reverse lists results top to bottom, mimicking the familiar layouts of git log, history, and env
        # border shows where the fzf window begins and ends
        # height=90% leaves space to see the current command and some scrollback, maintaining context of work
        # preview-window=wrap wraps long lines in the preview window, making reading easier
        # marker=* makes the multi-select marker more distinguishable from the pointer (since both default to >)
        set --export FZF_DEFAULT_OPTS '--bind=ctrl-a:select-all,ctrl-d:deselect-all,ctrl-t:toggle-all --cycle --layout=reverse --border --height=90% --preview-window=wrap --marker="*"'
    end

    fzf $argv
end

function upgrade_everything
    echo -s 'Begin upgrading ' @ (date '+%s')
    set -g _kd_ue_upgrade_cmds

    _update_everything

    set -l nothing_to_upgrade 1

    # mas
    _get_mas_upgrade_cmd
    if test $status -ne 0
        return $status
    end

    if test (count $_kd_ue_mas_upgrades) -gt 0; set nothing_to_upgrade 0; end
    echo -ne "* mas:\n"
    for item in $_kd_ue_mas_upgrades
        echo -ne "  "$item"\n"
    end

    echo

    # brew
    _get_brew_upgrade_cmd
    if test $status -ne 0
        return $status
    end

    if test (count $_kd_ue_brew_upgrades) -gt 0; set nothing_to_upgrade 0; end
    echo -ne "* brew:\n"
    for item in $_kd_ue_brew_upgrades
        echo -ne "  "$item"\n"
    end

    # do we have anything to update? 
    if test $nothing_to_upgrade -eq 1
        echo 'Nothing to upgrade...exiting'
        return 1
    end

    # Ready to upgrade
    echo
    echo "Commands:"
    for cmd in $_kd_ue_upgrade_cmds
        echo '-' (string replace -r '^.+? ' '' $cmd)
    end

    echo
    set -l confirm (read -n1 -P'Confirm? [Yn] ')
    if contains $confirm N n
        return 1
    end

    for cmd in $_kd_ue_upgrade_cmds
        eval $cmd &
    end
    wait

    return $status
end

function _update_everything
    set -l update_cmds "brew update"
    for cmd in $update_cmds
        eval $cmd &
    end
    wait
end

function _get_brew_upgrade_cmd
    set -g _kd_ue_brew_upgrades
    set selected_apps (
        brew outdated -v 2>&1 | \
            _fzf_wrapper \
                --multi \
                --ansi \
                --tiebreak=index \
                --prompt='brew: '
    )

    # echo $status
    # echo $selected_apps

    if test $status -ne 0
        return $status
    end

    set selected_apps (string split "\n" $selected_apps)
    # echo $selected_apps
    set app_ids
    set app_name_vers
    for app in $selected_apps
        # echo "app:" "'"$app"'"
        # [id, name + version]
        set app_infos (string split --max 1 ' ' $app)
        # echo 'app_infos:' $app_infos
        set -a app_ids (string match -r ".+" $app_infos[1])
        set -a app_name_vers (string join " " (string match -r ".+" $app_infos))
    end
    # echo "app_ids:" $app_ids
    # echo "app_name_vers:" $app_name_vers
    set -a _kd_ue_brew_upgrades $app_name_vers
    set -a _kd_ue_upgrade_cmds (string join " " command brew upgrade $app_ids)
end


function _get_mas_upgrade_cmd
    set -g _kd_ue_mas_upgrades
    set selected_apps (
        mas outdated 2>&1 | \
            _fzf_wrapper \
                --multi \
                --ansi \
                --tiebreak=index \
                --prompt='mas: '
    )

    # echo $status
    # echo $selected_apps

    if test $status -ne 0
        return $status
    end

    set selected_apps (string split "\n" $selected_apps)
    # echo $selected_apps
    set app_ids
    set app_name_vers
    for app in $selected_apps
        # echo "app:" "'"$app"'"
        # [id, name + version]
        set app_infos (string split --max 1 ' ' $app)
        # echo 'app_infos:' $app_infos
        set -a app_ids (string match -r '.+' $app_infos[1])
        set -a app_name_vers (string join " " (string match -r '.+' $app_infos[2]))
    end
    # echo "app_ids:" $app_ids
    # echo "app_name_vers:" $app_name_vers
    set -a _kd_ue_mas_upgrades $app_name_vers
    set -a _kd_ue_upgrade_cmds (string join " " command mas upgrade $app_ids)
end
