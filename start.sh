#!/usr/bin/env bash

set -eu

day=$1
year=${year-$(date +'%Y')}
lang=ruby

printf -v dir "$year/%02d" "$day"
mkdir -p "$dir"

cp -nR templates/$lang/* -t "$dir"
if [ "$lang" = ocaml ] && [ -f "$dir/dune" ]; then
  sed -i "s/\<year_n_day_n_template\>/year_${year}_day_${day}_solution/" "$dir/dune"
fi
