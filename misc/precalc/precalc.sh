#!/usr/bin/env bash

set -o pipefail

export BINDIR="${HOME}/dev/audacious-uade/src/plugin/cli"

export PROBE=1
export PLAY="${BINDIR}/player/player"
export PRECALC="${BINDIR}/precalc/precalc"
export INCLUDEPATH=$1
export TAC=$(which tac 2>/dev/null || echo tail -r)
export GSTRINGS=$(which gstrings 2>/dev/null || which strings)

# avoid "illegal byte sequence" issues
export LANG=C
export LC_ALL=C
export LC_CTYPE=C

unameSys="$(uname -s)"
case "${unameSys}" in
    Darwin*)
        if [ "$GSTRINGS" != "$(which gstrings 2>/dev/null)" ]; then
          echo "Please install gnu strings command (binutils)"
          exit 1
        fi
        ;;
    *)
        if [ -z "$GSTRINGS" ]; then
          echo "Please install strings command (binutils)"
          exit 1
        fi
        ;;
esac

run_uade() {
  local HOME="/tmp/songdb/$1"
  local WORK="$HOME"
  mkdir -p $WORK
  mkdir -p $WORK/strings
  echo "Processing $2" >> "$WORK/stderr"
  "$PRECALC" "$2" $INCLUDEPATH >> "$WORK/songdb.tsv" 2>> "$WORK/stderr"
  local RES=$?
  if [ "$RES" -gt "1" ]; then
    echo "Failed to process $2 - exit code $RES " | tee -a "$WORK/stderr"
  elif [ "$RES" -eq "0" ]; then
    local MD5=$(md5sum -b -- "$2" | head -c 32)
    local PLAYER="$(${PRECALC} "$2" player 2> /dev/null)"
    local SUBSONGS="$(${PRECALC} "$2" subsongs 2> /dev/null)"

    local AUDIO_CHROMAPRINT
    local AUDIO_MD5
    local AUDIO_BYTES

    local SUBSONG

    for SUBSONG in $SUBSONGS; do
      local SONGLENGTH_MILLIS=$($TAC "$WORK/songdb.tsv" | grep -a -m 1 "$MD5	$SUBSONG	" | cut -f 3)
      if [ $SONGLENGTH_MILLIS -le 0 ]; then
        echo -e $MD5'\t'$PLAYER'\t'$SUBSONG'\t'0 >> "$WORK/audio.tsv"
        continue
      fi
      {
        local FREQUENCY=11025
        if [ "$PLAYER" = "noisetrekker2" ] || [ "$PLAYER" = "protrekkr1" ] || [ "$PLAYER" = "protrekkr2" ]; then
          FREQUENCY=44100
        fi
        local SONGLENGTH=$(( SONGLENGTH_MILLIS / 1000 + 1))
        local MAX_BYTES=$((4 * $FREQUENCY * 1200))
        local BYTES=$(( SONGLENGTH > 1200 ? MAX_BYTES : 4 * $FREQUENCY * SONGLENGTH ))
        # XXX chromaprint -channels parameter does not work on gentoo (ffmpeg issue?), use sox to force mono
        ${PLAY} $FREQUENCY "$2" $SUBSONG 2> /dev/null \
        | head -c $BYTES \
        | sox -t raw -b 16 -e signed -c 2 -r $FREQUENCY - -t raw -b 16 -e signed -c 1 -r $FREQUENCY -D - remix 1-2 \
        | tee \
          >(wc -c | xargs >&2) \
          >(echo $(md5sum | head -c 32) >&2) \
          | fpcalc -length 9999 -rate $FREQUENCY -channels 1 -format s16le -plain - 2>/dev/null
      } 2>&1 | {
        read AUDIO_MD5
        read AUDIO_BYTES
        read AUDIO_CHROMAPRINT
        echo -e $MD5'\t'$PLAYER'\t'$SUBSONG'\t'$AUDIO_BYTES'\t'$AUDIO_MD5'\t'$AUDIO_CHROMAPRINT >> "$WORK/audio.tsv"
      }
    done
    local OMESSAGE=$(openmpt123 --info --message -- "$2" 2>/dev/null | grep -A 9999 '^Message....:' | sed 's/^.*: //g')
    local STRINGS=$OMESSAGE
    if [ -z "$STRINGS" ]; then
      STRINGS=$( "$GSTRINGS" -n 3 -- "$2" 2>/dev/null)
    fi
    rm -f "$WORK/strings/$MD5.strings"
    if [ -n "$OMESSAGE" ]; then
      local OTITLE=$(openmpt123 --info --message -- "$2" 2>/dev/null | grep '^Title......:' | sed 's/^.*: //g')
      echo "$OTITLE" >> "$WORK/strings/$MD5.strings"
    fi
    # if more than 4096, print first 2048 and last 2048
    if [ $(echo -n "$STRINGS" | wc -c) -gt 4096 ]; then
      echo "$STRINGS" | head -c 2048 >> "$WORK/strings/$MD5.strings"
      echo "" >> "$WORK/strings/$MD5.strings"
      echo "..." >> "$WORK/strings/$MD5.strings"
      echo "$STRINGS" | tail -c 2048 >> "$WORK/strings/$MD5.strings"
    else
      echo "$STRINGS" >> "$WORK/strings/$MD5.strings"
    fi
  fi
}

mkdir -p /tmp/songdb

export -f run_uade
find -L  . -type f | sed "s/^\.\///g" | parallel --nice 20 --timeout 7200 run_uade {%} {}

cat /tmp/songdb/*/songdb.tsv | sort | uniq > /tmp/songdb/songdb.tsv
cat /tmp/songdb/*/audio.tsv | sort | uniq > /tmp/songdb/audio.tsv
cat /tmp/songdb/*/stderr > /tmp/songdb/stderr
for i in 0 1 2 3 4 5 6 7 8 9 a b c d e f; do
  mkdir -p /tmp/songdb/strings/$i
  find -L /tmp/songdb -type f -path "*/strings/${i}*.strings" -exec mv -f -- "{}" /tmp/songdb/strings/"$i"/ \;
done
