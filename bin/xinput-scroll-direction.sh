#!/bin/bash

function reverse_scroll_direction() {
  local id
  for id in $(xinput list | grep 'slave *pointer' | grep -o 'id=[0-9]*')
  do
    xinput set-button-map ${id#id=} 1 2 3 5 4 7 6 8 9 10 11 12 13 14 15 16
  done
}

reverse_scroll_direction
