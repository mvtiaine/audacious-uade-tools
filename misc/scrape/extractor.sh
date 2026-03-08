#!/usr/bin/env bash

# avoid "illegal byte sequence" issues
export LANG=C
export LC_ALL=C
export LC_CTYPE=C

NPROC=$(getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)
GROUP=$(id -gn)

# wait/pid helper: manage pid-array and block until there is room (< nproc)
manage_pids_wait() {
  local -n _pids_ref=$1
  local nproc=${2:-$NPROC}
  while [ "${#_pids_ref[@]}" -ge "$nproc" ]; do
    wait -n >/dev/null 2>&1 || true
    for i in "${!_pids_ref[@]}"; do
      if ! kill -0 "${_pids_ref[i]}" 2>/dev/null; then
        wait "${_pids_ref[i]}" >/dev/null 2>&1 || true
        unset "_pids_ref[$i]"
      fi
    done
    _pids_ref=( "${_pids_ref[@]}" )
  done
}

run_extract() {
  local extract_cmd="$1"
  shift
  local suffixes="$*"
  local -a find_args=()
  local first=true
  for suffix in $suffixes; do
    if $first; then
      first=false
    else
      find_args+=("-o")
    fi
    find_args+=("-iname" "*${suffix}")
  done
  echo "Extracting $suffixes ..."
  local -a _pids=()
  while IFS= read -r -d $'\0' file
  do
    local src="$PWD/$file"
    local tmpdir="$(mktemp -d /tmp/extract.$1.XXXXXX)" || { echo "mktemp failed for: $src" >&2; continue; }
    (
      if (cd "$tmpdir" && eval "$extract_cmd"; chmod -R 755 .; chown -R ${USER}:${GROUP} .) >/dev/null 2>&1; then
        if [ -n "$(find "$tmpdir" -type f -size +0c -print -quit)" ]; then
          # XXX avoid "filename too long issues" due to deeply nested directories
          rm -rf -- "$tmpdir/+OKS IMPORT DIVISION-MEGAFORCE/"
          rm -rf -- "$tmpdir/ *** THE KENT TEAM *** /"
          rm -rf -- "$tmpdir/  * BAMIGA SECTOR ONE *  /"
          rm -rf -- "$tmpdir/THE KENT TEAM & Bamiga Sector /"
          rm -rf -- "$tmpdir/"*"/THE KENT TEAM & Bamiga Sector /"
          rm -rf -- "$tmpdir/       >>>      Iceman      <</"
          rm -rf -- "$tmpdir/     >>> TMF Crunchdisk 2.0 <</"
          rm -rf -- "$tmpdir/ ** THE BITSTOPPERS ** /"
          rm -f -- "$src"
          mv -- "$tmpdir" "$src" >/dev/null 2>&1
          chmod -R 755 "$src" >/dev/null 2>&1
          chown -R ${USER}:${GROUP} "$src" >/dev/null 2>&1
        else
          rm -rf -- "$tmpdir"
        fi
      else
        rm -rf -- "$tmpdir"
      fi
    ) &
    _pids+=("$!")
    manage_pids_wait _pids "$NPROC"
  done < <(gfind -L . -type f -size +0c \( "${find_args[@]}" \) -print0 2>/dev/null | grep -z -v '\.part[0-9]\+\.rar$')

  manage_pids_wait _pids 1
}

run_extract_pair() {
  local extract_cmd="$1"
  local primary_suffix="$2"   # e.g. .cue
  local secondary_suffix="$3" # e.g. .bin
  echo "Extracting paired ${primary_suffix} + ${secondary_suffix} ..."
  local -a _pids=()
  while IFS= read -r -d $'\0' file
  do
    local src="$PWD/$file"
    local base="${src%.*}"
    local dir="$(dirname "$src")"
    local stem="$(basename "$base")"
    # case-insensitive search for paired file
    local src2
    src2=$(gfind "$dir" -maxdepth 1 -type f -size +0c -iname "${stem}${secondary_suffix}" -print -quit 2>/dev/null)
    [ -z "$src2" ] && continue
    local tmpdir="$(mktemp -d /tmp/extract.pair.XXXXXX)" || { echo "mktemp failed for: $src" >&2; continue; }
    (
      if (cd "$tmpdir" && eval "$extract_cmd"; chmod -R 755 .; chown -R ${USER}:${GROUP} .) >/dev/null 2>&1; then
        if [ -n "$(find "$tmpdir" -type f -size +0c -print -quit)" ]; then
          rm -f -- "$src2"
          mv -- "$tmpdir" "$src2" >/dev/null 2>&1
          chmod -R 755 "$src2" >/dev/null 2>&1
          chown -R ${USER}:${GROUP} "$src2" >/dev/null 2>&1
        else
          rm -rf -- "$tmpdir"
        fi
      else
        rm -rf -- "$tmpdir"
      fi
    ) &
    _pids+=("$!")
    manage_pids_wait _pids "$NPROC"
  done < <(gfind -L . -type f -size +0c -iname "*${primary_suffix}" -print0 2>/dev/null)

  manage_pids_wait _pids 1
}

chmod -R 755 . >/dev/null 2>&1
chown -R ${USER}:${GROUP} . >/dev/null 2>&1

COUNT=$(gfind -L . -type d 2>/dev/null | wc -l)
while : ; do
  run_extract 'f=`mktemp /tmp/extract.iso.gz.XXXXXX`; gzip -d -c "$src" > "${f}" && 7z x -aoa "${f}"; rm -f "${f}"' \
    .iso.gz
  run_extract 'f=`mktemp /tmp/extract.iso.bz2.XXXXXX`; bzip2 -d -c "$src" > "${f}" && 7z x -aoa "${f}"; rm -f "${f}"' \
    .iso.bz2

  run_extract 'f=`mktemp /tmp/extract.mdf.XXXXXX`; mdf2iso "$src" "${f}"; 7z x -aoa "${f}"; rm -f "${f}"' \
    .mdf

  run_extract 'f=`mktemp /tmp/extract.nrg.XXXXXX`; nrg2iso "$src" "${f}"; 7z x -aoa "${f}"; rm -f "${f}"' \
    .nrg

  run_extract 'f=`mktemp /tmp/extract.img.xz.XXXXXX`; xz -d -c "$src" > "${f}" && 7z x -aoa "${f}"; rm -f "${f}"' \
    .img.xz

  run_extract '7z x -ppassword -aoa "$src"' \
    .dmg .7z .iso .img .image .exe

  run_extract_pair 'bchunk "$src2" "$src" track; for iso in track*.iso; do [ -f "$iso" ] && 7z x -aoa "$iso" && rm -f "$iso"; done' \
    .cue .bin

  run_extract_pair 'f=`mktemp /tmp/extract.img.XXXXXX`; ccd2iso "$src2" "${f}"; 7z x -aoa "${f}"; rm -f "${f}"' \
    .ccd .img
  
  run_extract 'f=`mktemp /tmp/extract.dmg.gz.XXXXXX`; gzip -d -c "$src" > "${f}" && 7z x -aoa "${f}"; rm -f "${f}"' \
    .dmg.gz

  run_extract 'f=`mktemp /tmp/extract.adf.XXXXXX`; unadf "$src"; chmod -R 755 .; chown -R ${USER}:${GROUP} .; readdisk "$src" "${f}"; cp -nrp "${f}"/*/. .; rm -rf "${f}"' \
    .adf \
    .d1 .d2 .d3 .d4 .d5 .d6 .d7 .d8 .d9 \
    disk.1 disk.2 disk.3 disk.4 disk.5 disk.6 disk.7 disk.8 disk.9 \
    disk1 disk2 disk3 disk4 disk5 disk6 disk7 disk8 disk9 \
    .hdf
  run_extract 'f=`mktemp /tmp/extract.hdf.gz.XXXXXX`; gzip -d -c "$src" > "${f}" && unadf "${f}"; rm -f "${f}"' \
    .hdf.gz

  run_extract 'f=`mktemp /tmp/extract.adz.XXXXXX`; f2=`mktemp -d /tmp/extract.adz.XXXXXX`; gzip -d -c "$src" > "${f}" && (unadf "${f}"; chmod -R 755 .; chown -R ${USER}:${GROUP} .; readdisk "${f}" "${f2}"; cp -nrp "${f2}"/*/. .); rm -f "${f}"; rm -rf "${f2}"' \
    .adz .adf.gz

  run_extract 'f=`mktemp /tmp/extract.dms.XXXXXX`; f2=`mktemp -d /tmp/extract.dms.XXXXXX`; ancient d "$src" "${f}" && (unadf "${f}"; chmod -R 755 .; chown -R ${USER}:${GROUP} .; readdisk "${f}" "${f2}"; cp -nrp "${f2}"/*/. .); rm -f "${f}"; rm -rf "${f2}"' \
    .dms

  run_extract 'tar xvf "$src"' \
    .tgz .tbz2 .txz .tz .tlz .gz .bz2 .xz .Z .lzma .tar

  run_extract 'f=`mktemp -d /tmp/extract.rar.XXXXXX`; 7z x -ppassword -aoa "$src"; chmod -R 755 .; chown -R ${USER}:${GROUP} .; unrar x -ppassword -o+ -op"${f}" "$src"; chmod -R 755 "${f}"; chown -R ${USER}:${GROUP} "${f}"; cp -rp "${f}"/. .; rm -rf "${f}"' \
    .rar
  run_extract 'f=`mktemp -d /tmp/extract.zip.XXXXXX`; 7z x -ppassword -aoa "$src"; chmod -R 755 .; chown -R ${USER}:${GROUP} .; unzip -P password -d "${f}" -o "$src"; chmod -R 755 "${f}"; chown -R ${USER}:${GROUP} "${f}"; cp -rp "${f}"/. .; rm -rf "${f}"' \
    .zip
  run_extract 'f=`mktemp -d /tmp/extract.lha.XXXXXX`; 7z x -aoa "$src"; chmod -R 755 .; chown -R ${USER}:${GROUP} .; lha x -f -w="${f}" "$src"; chmod -R 755 "${f}"; chown -R ${USER}:${GROUP} "${f}"; cp -rp "${f}"/. .; rm -rf "${f}"' \
    .lha .lzh
  run_extract 'lhasa xf "$src"' \
    .lha .lzh
  run_extract 'unlzx "$src"' \
    .lzx .lz
  run_extract 'unarj x "$src"' \
    .arj
  run_extract 'f=`mktemp -d /tmp/extract.cab.XXXXXX`; 7z x -aoa "$src"; chmod -R 755 .; chown -R ${USER}:${GROUP} .; cabextract -d "${f}" "$src"; chmod -R 755 "${f}"; chown -R ${USER}:${GROUP} "${f}"; cp -rp "${f}"/. .; rm -rf "${f}"' \
    .cab
  run_extract 'unace x "$src"' \
    .ace
  run_extract 'zoo x "$src"' \
    .zoo

  NEW_COUNT=$(gfind -L . -type d 2>/dev/null | wc -l)
  [[ $NEW_COUNT -eq $COUNT ]] && break
  COUNT=$NEW_COUNT
done

chmod -R 755 . >/dev/null 2>&1
chown -R ${USER}:${GROUP} . >/dev/null 2>&1

echo "Extracting .gz .bz2 .xz .Z .lzma ..."

_pids=()
while IFS= read -r -d $'\0' file
do
  src="$PWD/$file"
  tmpdir="$(mktemp -d /tmp/extract.compressed.XXXXXX)" || { echo "mktemp failed for: $src" >&2; continue; }
  filename="$(basename "$src")"
  (
    shopt -s nocasematch
    if (cd "$tmpdir" && case "$src" in
      *.gz) gzip -d -c "$src" > "${filename}" ;;
      *.bz2) bzip2 -d -c "$src" > "${filename}" ;;
      *.xz) xz -d -c "$src" > "${filename}" ;;
      *.Z) uncompress -c "$src" > "${filename}" ;;
      *.lzma) lzma -d -c "$src" > "${filename}" ;;
      *) echo "Unsupported compressed file: $src" >&2; exit 1 ;;
    esac; chmod -R 755 .; chown -R ${USER}:${GROUP} .) >/dev/null 2>&1; then
      if [ -n "$(find "$tmpdir" -type f -size +0c -print -quit)" ]; then
        rm -f -- "$src"
        mv -- "$tmpdir/$filename" "$src" >/dev/null 2>&1
        rm -rf -- "$tmpdir"
      else
        rm -rf -- "$tmpdir"
      fi
    else
      rm -rf -- "$tmpdir"
    fi
  ) &
  _pids+=("$!")
  manage_pids_wait _pids "$NPROC"
done < <(gfind -L . -type f -size +0c -iname "*.gz" -o -iname "*.bz2" -o -iname "*.xz" -o -iname "*.Z" -o -iname "*.lzma" -print0 2>/dev/null)

manage_pids_wait _pids 1

# ancient (single file)
for i in {1..2}; do
  echo "Extracting ancient ($i/2) ..."
  gfind -L . -type f -size +0c -print0 | xargs -0 -P "$NPROC" -n 1 ancient i 2>&1 \
  | grep \
    -e '^Compression of ' \
  | gsed \
    -e s/'^Compression of '//g \
    -e s/'^Unknown or invalid compression format in file '//g \
    -e s/' is \S*: '.*//g \
    -e s/' is <invalid>$'//g \
  | grep \
    -v -e '\.[dD][mM][sS]$' \
    -v -e '\.[aA][dD][fF]\.[a-zA-Z]\?[zZ][0-9]\?$' \
    -v -e '\.[aA][dD][zZ]$' \
    -v -e '\.[lL][hH][aA]$' \
    -v -e '\.[lL][zZ][hH]$' \
    -v -e '\.[tT][gG][zZ]$' \
    -v -e '\.[tT][aA][rR]\.[a-zA-Z]\?[zZ][0-9]\?$' \
    -v -e '\.[iI][sS][oO]\.[a-zA-Z]\?[zZ][0-9]\?$' \
    -v -e '\.[dD][mM][gG]\.[a-zA-Z]\?[zZ][0-9]\?$' \
    -v -e '\.[hH][dD][fF]\.[a-zA-Z]\?[zZ][0-9]\?$' \
    -v -e ' is SCO Compress LZH$' \
  > ancient.files

  _pids=()
  while IFS= read -r file
  do
    src="$PWD/$file"
    tmpdir="$(mktemp -d /tmp/extract.ancient.XXXXXX)" || { echo "mktemp failed for: $src" >&2; continue; }
    filename="$(basename "$src")"
    (
      if (cd "$tmpdir" && ancient -p d "$src" "$tmpdir/$filename") >/dev/null 2>&1; then
        if [ -n "$(find "$tmpdir" -type f -size +0c -print -quit)" ]; then
          rm -f -- "$src"
          mv -- "$tmpdir/$filename" "$src" >/dev/null 2>&1
          rm -rf -- "$tmpdir"
        else
          rm -rf -- "$tmpdir"
        fi
      else
        rm -rf -- "$tmpdir"
      fi
    ) &
    _pids+=("$!")
    manage_pids_wait _pids "$NPROC"
  done < ancient.files

  manage_pids_wait _pids 1
done

chmod -R 755 . >/dev/null 2>&1
chown -R ${USER}:${GROUP} . >/dev/null 2>&1
