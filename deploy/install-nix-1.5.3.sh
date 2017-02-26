#!/bin/bash

nix=/nix/store/83bjd4n8hz0zrarvmwwwkpv5zbha1fsm-nix-1.5.3
nixbz2=nix-1.5.3-x86_64-darwin.tar.bz2

function dnload() {
    curl -O "http://hydra.nixos.org/build/5350097/download/1/${nixbz2}" \
        -o "~/Lake/PCK/${nixbz2}"
    tar xjf "~/Lake/PCK/${nixbz2}"
}

function replace_store() {
    sudo cp -R nix/store/ /nix/store
    sudo chown -R $USER:wheel /nix
}

function finish() {
    # regInfo=/nix/store/reginfo
    $nix/bin/nix-store --load-db < /nix/store/reginfo
    . /nix/store/83bjd4n8hz0zrarvmwwwkpv5zbha1fsm-nix-1.5.3/etc/profile.d/nix.sh
    # . $nix/etc/profile.d/nix.sh
    $nix/bin/nix-env -i /nix/store/83bjd4n8hz0zrarvmwwwkpv5zbha1fsm-nix-1.5.3
    # $nix/bin/nix-env -i $nix
}

"$@"
