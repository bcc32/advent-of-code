#!/usr/bin/env bash

set -eu

day=$1
curl --cookie cookies.txt "https://adventofcode.com/2015/day/$day/input"
