#!/usr/bin/env bash

set -e

export PRECALC=$(dirname "$0")/precalc.sh

for i in *; do
  if [[ -d "$i" && ! -f "$i/.meta" && ! -d "$HOME/tsv/$i" ]]; then
    echo 
    echo "Precalculating $i ..."
    echo
    (cd "$i" && $PRECALC yes)
    mkdir -p "$HOME/tsv/$i"
    mv /tmp/songdb/*.tsv /tmp/songdb/stderr /tmp/songdb/strings "$HOME/tsv/$i/"
    rm -rf /tmp/songdb/*
  fi
done
