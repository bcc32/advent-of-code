#!/bin/sh

set -euo pipefail

for i in $(seq 1 25); do
    name="$(printf "%02d" $i)"
    if [ ! -e "$name" ]; then
        cp -r template "$name"
        break
    fi
done
