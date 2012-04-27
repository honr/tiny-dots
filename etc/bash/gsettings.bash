_gsettings()
{
    local cur L

    COMPREPLY=()
    cur=`_get_cword`

    if [[ "$COMP_CWORD" == "1" ]]; then
        COMPREPLY=( $( compgen -W 'help list-schemas list-relocatable-schemas list-keys list-children list-recursively range get set reset reset-recursively writable monitor' -- "$cur" ) )
    elif [[ "$COMP_CWORD" == "2" ]]; then
	case "${COMP_WORDS[1]}" in 
	    list-keys|list-children|list-recursively|range|get|set|reset|reset-recursively|writable|monitor)
		mapfile -t L < <(gsettings list-schemas)
		L=("${L[@]# }")
		COMPREPLY=( $( compgen -W "${L[*]}" -- "$cur" ) )
		;;
	esac
    elif [[ "$COMP_CWORD" == "3" ]]; then
	case "${COMP_WORDS[1]}" in 
	    range|get|set|reset|writable|monitor)
		mapfile -t L < <(gsettings list-keys "${COMP_WORDS[2]}")
		L=("${L[@]# }")
		COMPREPLY=( $( compgen -W "${L[*]}" -- "$cur" ) )
		;;
	esac
    fi
}

complete -F _gsettings gsettings
