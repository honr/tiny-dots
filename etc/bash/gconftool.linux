
_gconftool_dir ()
{
    local prev cur L K
    cur="${1##*/}"
    prev="${1%/*}"
    if [ -z "$prev" ] ; then prev="/" ; fi
    mapfile -t L < <(gconftool --all-dirs "$prev")
    L=("${L[@]# }")
    COMPREPLY=( $( compgen -W "${L[*]}" -- "$1" ) )
    if [ "${#COMPREPLY[@]}" -eq 1 ] ; then
	K="$(gconftool --all-dirs "${COMPREPLY[0]}")"
	if [ -n "$K" ] ; then
	    COMPREPLY[1]="${COMPREPLY[0]}/"
	fi
    fi
}

_gconftool_node ()
{
    local prev cur L
    cur="${1##*/}"
    prev="${1%/*}"
    if [ -z "$prev" ] ; then prev="/" ; fi
    mapfile -t L < <(gconftool --all-dirs "$prev")
    if [ "${#L[@]}" -eq 0 ] ; then
	mapfile -t L < <(gconftool --all-entries "$prev")
	L=("${L[@]# }")
	L=("${L[@]% =*}")
	L=("${L[@]/#/$prev/}")
	COMPREPLY=( $( compgen -W "${L[*]}" -- "$1" ) )
    else
	L=("${L[@]# }")
	COMPREPLY=( $( compgen -W "${L[*]}" -- "$1" ) )
	if [ "${#COMPREPLY[@]}" -eq 1 ] ; then
	    COMPREPLY[1]="${COMPREPLY[0]}/"
	fi
    fi
}


_gconftool()
{
    local cur prev split=false

    COMPREPLY=()
    cur=`_get_cword`
    prev=${COMP_WORDS[COMP_CWORD-1]}

    _split_longopt && split=true

    case "$prev" in
	-s|--set|-g|--get|-u|--unset|--toggle|--short-docs|--long-docs|-T|--get-type|--get-list-size|--get-list-element|--set-schema|--get-schema-name|--apply-schema|--unapply-schema)
	    _gconftool_node "$cur"
            return 0
	    ;;
	--recursive-unset|-a|--all-entries|--all-dirs|-R|--recursive-list|--dir-exists)
	    _gconftool_dir "$cur"
            return 0
	    ;;
	-S|--search-key|--search-key-regex|--ignore-schema-defaults|-t|--type|--list-type|--car-type|--cdr-type|--dump|--load|--unload|--get-default-source|--shutdown|-p|--ping|--spawn|--install-schema-file|--config-source|--direct|--makefile-install-rule|--makefile-uninstall-rule|--break-key|--break-directory)
            # COMPREPLY=( 'completion-not-implemented-yet' )
            return 0
            ;;
    esac

    $split && return 0

    if [[ "$cur" == -* ]]; then
        COMPREPLY=( $( compgen -W '-h --help --help-all --help-client --help-key-type --help-load --help-server --help-install --help-test --help-schema -s --set -g --get -u --unset --recursive-unset --toggle -a --all-entries --all-dirs -R --recursive-list -S --search-key --search-key-regex --short-docs --long-docs --dir-exists --ignore-schema-defaults -t --type -T --get-type --get-list-size --get-list-element --list-type --car-type --cdr-type --dump --load --unload --get-default-source --shutdown -p --ping --spawn --install-schema-file --config-source --direct --makefile-install-rule --makefile-uninstall-rule --break-key --break-directory --set-schema --short-desc --long-desc --owner --get-schema-name --apply-schema --unapply-schema -v --version ' -- "$cur" ) )
    else
        COMPREPLY=()
	# $( compgen -W '-h -v' -- "$cur" ) )
    fi

}
complete -F _gconftool gconftool

