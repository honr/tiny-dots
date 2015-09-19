function cloj () {
  clove -c clojure "$@"
}

function cloji () {
  clove -i clojure "$@"
}

function sx () {
  if [ -z "$1" ] ; then
    screen -ls
  else
    STY="$1" screen -xR "$1"
  fi
}

function e.which(){
  emacsclient -nc $(which $1)
}
complete -c e.which

function e.e () {
  if [ -z "$1" ] ; then
      emacsclient -nc .
  else
    emacsclient -nc "$@"
  fi
}

function e.sx () {
  if [ -z "$STY" ] ; then
      return
  else
    emacsclient -n $(cat "$HOME/.screen/${STY##*.}.files")
  fi
}

function e.bell () {
  echo -e '\a'
}

function e.cool! () {
  tail -1 $HOME/.history/bash >>$HOME/.history/bash.cool
}

alias less='less -Ri'

function e.notify-desktop () {
  local summary="$1"
  shift
  notify-send -i terminal "$summary" "$@"
}

function e.notify-mail () {
  local summary="$1"
  shift
  echo -e "$@" | mailx -s "[e.notify-mail] $summary" "$USER_EMAIL_ADDRESS"
}

export __stopwatch_start_time

function e.tic () {
  __stopwatch_start_time="$(date +%s)"
}

function e.toc () {
  # TODO: -b -> notify by terminal bell. -i -> notify by IM.
  local exit_code="$?"
  local notification_fn="e.notify-desktop"
  case "$1" in
    -m) notification_fn="e.notify-mail" ; shift ;;
    -i) notification_fn="e.notify-im" ; shift ;;  # Not implemented.
    -d) notification_fn="e.notify-desktop" ; shift ;;
    -*) echo "Unknown flag" ; shift ;;
  esac
  local summary="«$exit_code» - $1"
  shift
  local body="$@"
  body+="\nExit code: ${exit_code}."
  body+="  Took $(($(date +%s) - __stopwatch_start_time)) seconds."
  ${notification_fn} "$summary" "$body"
  return ${exit_code}
}

alias la='ls -larth'
