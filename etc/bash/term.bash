function git_current_branch () {
  local current_branch="$(git symbolic-ref -q HEAD 2>/dev/null)"
  if [ -n "$current_branch" ] ; then
      current_branch="${current_branch##refs/heads/}"
      current_branch="${current_branch:-HEAD}"
      echo "$current_branch"
  fi
}

function git_current_branch_str  () {
  local current_branch="$(git_current_branch)"
  if [ -n "$current_branch" ] ; then
      echo "[${current_branch}]"
  fi
}

export PS1_SRC=''
function prepare-for-prompt-update-ps1 () {
  local use_color_prompt
  local plain_prompt='{{num}}. {{chroot}}\u@\h:$(git_current_branch_str)\w #{{date}}'
  case "$TERM" in
    xterm-*color) use_color_prompt=t;;
    screen-*color) use_color_prompt=t;;
  esac

  if [ -z "$use_color_prompt" ]; then
      PS1_SRC="${plain_prompt} "
  else
    PS1_SRC='\[\033[1;33m\]{{num}}. #{{date}}\n\[\033[01;33m\]{{chroot}}\[\033[01;32m\]\u@\h:$(git_current_branch_str)\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '

    case "$TERM" in
      xterm*|rxvt*)
        PS1_SRC+="\[\033]0;${plain_prompt}  +Term\007\]" # Set the title
        ;;
    esac

    export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:'
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
  fi
}

PS1_line_number=0
function prompt-update-ps1 () {
  ((PS1_line_number++))
  PS1="${PS1_SRC//'{{date}}'/$(date +%Y-%m-%d\ %H:%M:%S)}"
  PS1="${PS1//'{{num}}'/$PS1_line_number}"
  PS1="${PS1//'{{chroot}}'/${debian_chroot:+($debian_chroot)}}"
}

[[ -t 0 && -t 1 ]] && stty werase ^-
