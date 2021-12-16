#!/usr/bin/env bash

set -eu

day=$1
year=2021

printf -v dir "$year/%02d" "$day"
mkdir -p "$dir"

cp -nR templates/ruby/* -t "$dir"
curl -fsSL --cookie cookies.txt "https://adventofcode.com/$year/day/$day/input" -o "$dir/input"
