#!/bin/bash

function reverse_scroll_direction() {
  local mice=(
    'Logitech USB Optical Mouse'
    'Mitsumi Electric Apple Optical USB Mouse'
    'HID-compliant Mouse HID-compliant Mouse'
  )
  local mouse
  for mouse in "${mice[@]}"; do
    local mouse_id="$(xinput list --id-only "pointer:$mouse" 2>/dev/null)"
    if [ -n "$mouse_id" ] ; then
      echo "Mouse '$mouse' found."
      xinput set-button-map "$mouse_id" 1 2 3 5 4 7 6 8 9 10 11 12 13 14 15 16
    fi
  done
}

reverse_scroll_direction
