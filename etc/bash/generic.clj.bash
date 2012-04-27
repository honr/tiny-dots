#!bash

alias _clj.comp='clove -c clojure -m rose.clu'

_clj.bin () {
    local x
    for x in $HOME/.local/bin/*.clj ; do
	complete -C _clj.comp "$(basename "$x")"
    done
}
_clj.bin
