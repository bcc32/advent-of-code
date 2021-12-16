#!/usr/bin/env bash

set -eu

day=$1
day_for_filename=$(printf "day_%02d" "$day")
cp -RT template "$day_for_filename"
sed -i "s/day_n_template/${day_for_filename}_solution/" "${day_for_filename}/dune"
curl --cookie cookies.txt "https://adventofcode.com/2020/day/$day/input" -o "${day_for_filename}/input.txt"
