#!/bin/bash

for mouse in \
    'Logitech USB Optical Mouse' \
    'Mitsumi Electric Apple Optical USB Mouse'
do
    xinput set-button-map "$mouse" 1 2 3 5 4 7 6 8 9 10 11 12 13 14 15 16
done
