#!/bin/sh

while read -r line; do
  DIR=$(echo $line | cut -c 7-9999)
  # check if DIR contains two slashes
  if [ $(echo $DIR | grep -o "/" | wc -l) -eq 2 ]; then
    # create the first directory
    mkdir -p "$(echo $DIR | cut -d '/' -f 1)"
    ln -s "../../modules/$DIR" "$(echo $DIR | cut -d '/' -f 1)/$(echo $DIR | cut -d '/' -f 2)"
  else
    ln -s "../modules/$DIR"
  fi
done < $(dirname -- "$0")/modland_amiga_dirs.txt
