#!/bin/sh

# NOTE: this is obsolete as modland allmods_md5.txt is no longer used

grep -E -f modland_amiga_dirs.txt allmods_md5.txt > allmods_md5_amiga.txt
# cut -c 34-9999 allmods_md5_amiga.txt | sort > allmods_amiga_sorted.txt
