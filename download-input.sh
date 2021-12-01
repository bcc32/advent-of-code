#!/usr/bin/env bash

set -eu

day=$1
year=2021

printf -v dir "%02d" "$day"
mkdir -p "$dir"

curl --cookie cookies.txt "https://adventofcode.com/$year/day/$day/input" -o "$dir/input"
