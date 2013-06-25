#!/bin/bash

curl -O http://hydra.nixos.org/build/5350097/download/1/nix-1.5.3-x86_64-darwin.tar.bz2
tar xjf nix-1.5.3-x86_64-darwin.tar.bz2
nix=/nix/store/83bjd4n8hz0zrarvmwwwkpv5zbha1fsm-nix-1.5.3
sudo cp -R nix/nix/store /nix/store
sudo chown -R honar:wheel /nix
$nix/bin/nix-store --load-db < /nix/store/reginfo
. $nix/etc/profile.d/nix.sh
$nix/bin/nix-env -i $nix
