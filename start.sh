#!/usr/bin/env bash

set -eu

day=$1
cp -R template "day_$day"
sed -i "s/day_n_template/day_${day}_solution/" "day_$day/dune"
curl --cookie cookies.txt "https://adventofcode.com/2015/day/$day/input" -o "day_$day/input.txt"
