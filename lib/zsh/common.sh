# `echo' with color codes, and with neither escaping (-e) nor (-n). Example:
# cecho -521: Hello -20:4 World -
cecho() {
  local arg
  for arg; do
    if [[ "$arg" == "-" ]] ; then # Reset colors.
        echo -ne '\033[0m'
    elif [[ "$arg" =~ ^-[0-9]{0,3}:[0-9]{0,3}$ ]] ; then
        arg="${arg/-/}"
        __cecho_helper "\\033[38;5;" "${arg%%:*}"
        __cecho_helper "\\033[48;5;" "${arg##*:}"
    else
      echo -n "$arg"
    fi
  done
  echo
}
__cecho_helper() {
  local prefix="$1"
  local color="$2"
  if [[ -z "$color" ]]; then return; fi
  if [[ "${#color}" == 3 ]] ; then
      echo -ne "${prefix}$(( 6#$color + 16 ))m"
  else
    echo -ne "${prefix}$(( 10#$color + 232 ))m"
  fi
}
