function ew(){
    emacsclient -nc $(which $1)
}

complete -c ew
