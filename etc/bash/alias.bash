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
