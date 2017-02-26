#!/bin/bash

export HISTSIZE=16384
export HISTFILE=$HOME/.history/bash
unset HISTFILESIZE
export HISTCONTROL=ignoreboth
export VISUAL="emacsclient"
export EDITOR="emacsclient"
export CLICOLOR=1

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
# HISTCONTROL=ignoredups:ignorespace

shopt -s histappend
shopt -s checkwinsize
set +o histexpand

function git_current_branch () {
  local current_branch="$(git symbolic-ref -q HEAD 2>/dev/null)"
  if [ -n "$current_branch" ] ; then
      current_branch="${current_branch##refs/heads/}"
      current_branch="${current_branch:-HEAD}"
      echo "$current_branch"
  fi
}

# Redefine to customize.
function current_workspace () {
  echo ""
}

export PS1_SRC=''
function prompt_update_init () {
  local use_color_prompt
  local plain_prompt='{{chroot}}\u@\h:{{branch}}\w {{date}}'
  case "$TERM" in
    xterm-*color) use_color_prompt=t;;
    screen-*color|screen) use_color_prompt=t;;
  esac

  if [ -z "$use_color_prompt" ]; then
      PS1_SRC="${plain_prompt}> "
  else
    PS1_SRC='\[\033[1;33m\]    {{date}}  ><(((°>\[\033[00m\]\n'
    PS1_SRC+='\[\033[01;36m\]{{chroot}}\[\033[01;32m\]{{num}}. \u@\h:'
    PS1_SRC+='\[\033[36m\]{{branch}}\[\033[01;34m\]\W\[\033[00m\]$ '
    case "$TERM" in
      xterm*|rxvt*)
        PS1_SRC+="\[\033]0;${plain_prompt}  +Term\007\]" # Set the title
        ;;
    esac
  fi
}

PS1_line_number=0
function prompt_update_ps1 () {
  ((PS1_line_number++))
  PS1="${PS1_SRC//'{{date}}'/$(date +%Y-%m-%d\ %H:%M:%S)}"
  PS1="${PS1//'{{num}}'/$PS1_line_number}"
  PS1="${PS1//'{{chroot}}'/${debian_chroot:+($debian_chroot)}}"
  local branch="$(current_workspace)"
  branch+="${branch:+ }$(git_current_branch)"
  PS1="${PS1//'{{branch}}'/${branch:+[$branch]}}"
}

[[ -t 0 && -t 1 ]] && stty werase ^-
