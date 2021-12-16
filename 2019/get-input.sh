#!/bin/sh

set -eu

year=2019
day=$1
session=$(cat "$(dirname "$0")/session.txt")

curl "https://adventofcode.com/$year/day/$day/input" --cookie "session=$session"
