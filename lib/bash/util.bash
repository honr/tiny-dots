#!/bin/bash

# echo with color codes interleaved
# example:
# e.cho @521 Hello @20/04 World
function e.cho() {
  local reset_color='\033[0m'
  local arg color
  for arg; do
    if [[ "$arg" =~ ^@[0-9]{2,3}$ ]] ; then
        local fg="${arg/@/}"
        if [[ ${#fg} == 2 ]] ; then
            fg="$(( 10#$fg + 232 ))"
        else
          fg="$(( 6#$fg + 16 ))"
        fi
        color="\\033[38;5;${fg}m"
    elif [[ "$arg" =~ ^@[0-9]{2,3}[/][0-9]{2,3}$ ]] ; then
        arg="${arg/@/}"
        local fg="${arg%%/*}"
        if [[ ${#fg} == 2 ]] ; then
            fg="$(( 10#$fg + 232 ))"
        else
          fg="$(( 6#$fg + 16 ))"
        fi
        local bg="${arg##*/}"
        if [[ ${#bg} == 2 ]] ; then
            bg="$(( 10#$bg + 232 ))"
        else
          bg="$(( 6#$bg + 16 ))"
        fi
        color="\\033[38;5;${fg}m\\033[48;5;${bg}m"
    elif [[ "$arg" =~ ^@[0-9]$ ]] ; then
        local fg="${arg/@/}"
        color="\\033[3${fg}m"
    elif [[ "$arg" == "@" ]] ; then
        color="$reset_color"
    else # Not a color
      if [[ -n "$color" ]] ; then
          echo -ne "$color"
      fi
      echo -n "$arg"
      if [[ -n "$color" ]] ; then
          echo -ne "$reset_color"
      fi
      color=""
    fi
  done
  echo
}

export -f e.cho
