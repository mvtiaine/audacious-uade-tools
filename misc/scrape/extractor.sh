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
  local suffix="$1"
  local extract_cmd="$2"
  echo "Extracting *.$suffix ..."
  local -a _pids=()
  while IFS= read -r -d $'\0' file
  do
    local src="$PWD/$file"
    local tmpdir="$(mktemp -d /tmp/extract.$suffix.XXXXXX)" || { echo "mktemp failed for: $src" >&2; continue; }
    (
      if (cd "$tmpdir" && eval "$extract_cmd"; chmod -R 755 .; chown -R ${USER}:${GROUP} .) >/dev/null 2>&1; then
        if [ -n "$(find "$tmpdir" -type f -size +0c -print -quit)" ]; then
          # XXX avoid "filename too long issues" due to deeply nested directories
          rm -rf -- "$tmpdir"/*/"+OKS IMPORT DIVISION-MEGAFORCE/"
          rm -rf -- "$tmpdir"/*/" *** THE KENT TEAM *** /"
          rm -rf -- "$tmpdir"/*/"  * BAMIGA SECTOR ONE *  /"
          rm -rf -- "$tmpdir"/*/"THE KENT TEAM & Bamiga Sector /"
          rm -rf -- "$tmpdir/       >>>      Iceman      <</"
          rm -rf -- "$tmpdir/     >>> TMF Crunchdisk 2.0 <</"
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
  done < <(find -L . -type f -iname "*.$suffix" -print0)

  manage_pids_wait _pids 1
}

chmod -R 755 . >/dev/null 2>&1
chown -R ${USER}:${GROUP} . >/dev/null 2>&1

COUNT=$(find -L . -type d | wc -l)
while : ; do
  run_extract dms 'f=`mktemp /tmp/extract.dms.XXXXXX`; f2=`mktemp -d /tmp/extract.dms.XXXXXX`; ancient d "$src" "${f}" && (unadf "${f}"; chmod -R 755 .; chown -R ${USER}:${GROUP} .; readdisk "${f}" "${f2}"; cp -nrp "${f2}"/*/. .); rm -f "${f}"; rm -rf "${f2}"'

  run_extract adz 'f=`mktemp /tmp/extract.adz.XXXXXX`; gzip -d -c "$src" > "${f}" && unadf "${f}"; rm -f "${f}"'
  run_extract adf.gz 'f=`mktemp /tmp/extract.adf.gz.XXXXXX`; gzip -d -c "$src" > "${f}" && unadf "${f}"; rm -f "${f}"'
  run_extract adf.bz2 'f=`mktemp /tmp/extract.adf.bz2.XXXXXX`; bzip2 -d -c "$src" > "${f}" && unadf "${f}"; rm -f "${f}"'
  run_extract adf.xz 'f=`mktemp /tmp/extract.adf.xz.XXXXXX`; xz -d -c "$src" > "${f}" && unadf "${f}"; rm -f "${f}"'
  run_extract adf.Z 'f=`mktemp /tmp/extract.adf.Z.XXXXXX`; uncompress -c "$src" > "${f}" && unadf "${f}"; rm -f "${f}"'
  run_extract adf 'unadf "$src"'

  run_extract hdz 'f=`mktemp /tmp/extract.adz.XXXXXX`; gzip -d -c "$src" > "${f}" && unadf "${f}"; rm -f "${f}"'
  run_extract hdf.gz 'f=`mktemp /tmp/extract.hdf.gz.XXXXXX`; gzip -d -c "$src" > "${f}" && unadf "${f}"; rm -f "${f}"'
  run_extract hdf.bz2 'f=`mktemp /tmp/extract.hdf.bz2.XXXXXX`; bzip2 -d -c "$src" > "${f}" && unadf "${f}"; rm -f "${f}"'
  run_extract hdf.xz 'f=`mktemp /tmp/extract.hdf.xz.XXXXXX`; xz -d -c "$src" > "${f}" && unadf "${f}"; rm -f "${f}"'
  run_extract hdf.Z 'f=`mktemp /tmp/extract.hdf.Z.XXXXXX`; uncompress -c "$src" > "${f}" && unadf "${f}"; rm -f "${f}"'
  run_extract hdf 'unadf "$src"'

  run_extract lha 'f=`mktemp -d /tmp/extract.lha.XXXXXX`; 7z x -aoa "$src"; chmod -R 755 .; chown -R ${USER}:${GROUP} .; lha x -f -w="${f}" "$src"; chmod -R 755 "${f}"; chown -R ${USER}:${GROUP} "${f}"; cp -rp "${f}"/. .; rm -rf "${f}"'
  run_extract lzh 'f=`mktemp -d /tmp/extract.lzh.XXXXXX`; 7z x -aoa "$src"; chmod -R 755 .; chown -R ${USER}:${GROUP} .; lha x -f -w="${f}" "$src"; chmod -R 755 "${f}"; chown -R ${USER}:${GROUP} "${f}"; cp -rp "${f}"/. .; rm -rf "${f}"'
  run_extract lzx 'unlzx "$src"'
  run_extract zip 'f=`mktemp -d /tmp/extract.zip.XXXXXX`; 7z x -aoa "$src"; chmod -R 755 .; chown -R ${USER}:${GROUP} .; unzip -d "${f}" -o "$src"; chmod -R 755 "${f}"; chown -R ${USER}:${GROUP} "${f}"; cp -rp "${f}"/. .; rm -rf "${f}"'
  run_extract 7z '7z x -aoa "$src"'
  run_extract rar 'f=`mktemp -d /tmp/extract.rar.XXXXXX`; 7z x -aoa "$src"; chmod -R 755 .; chown -R ${USER}:${GROUP} .; unrar x -o+ -op"${f}" "$src"; chmod -R 755 "${f}"; chown -R ${USER}:${GROUP} "${f}"; cp -rp "${f}"/. .; rm -rf "${f}"'
  run_extract arj 'unarj x "$src"'

  run_extract tgz 'tar xvzf "$src"'
  run_extract tar.gz 'tar xvzf "$src"'
  run_extract tar.bz2 'tar xvjf "$src"'
  run_extract tar.xz 'tar xvJf "$src"'
  run_extract tar.Z 'tar xvZf "$src"'
  run_extract tar 'tar xvf "$src"'

  run_extract iso.gz 'f=`mktemp /tmp/extract.iso.gz.XXXXXX`; gzip -d -c "$src" > "${f}" && 7z x -aoa "${f}"; rm -f "${f}"'
  run_extract iso.bz2 'f=`mktemp /tmp/extract.iso.bz2.XXXXXX`; bzip2 -d -c "$src" > "${f}" && 7z x -aoa "${f}"; rm -f "${f}"'
  run_extract iso.xz 'f=`mktemp /tmp/extract.iso.xz.XXXXXX`; xz -d -c "$src" > "${f}" && 7z x -aoa "${f}"; rm -f "${f}"'
  run_extract iso.Z 'f=`mktemp /tmp/extract.iso.Z.XXXXXX`; uncompress -c "$src" > "${f}" && 7z x -aoa "${f}"; rm -f "${f}"'
  run_extract iso '7z x -aoa "$src"'

  run_extract mdf.gz 'f=`mktemp /tmp/extract.mdf.gz.XXXXXX`; gzip -d -c "$src" > "${f}" && 7z x -aoa "${f}"; rm -f "${f}"'
  run_extract mdf.bz2 'f=`mktemp /tmp/extract.mdf.bz2.XXXXXX`; bzip2 -d -c "$src" > "${f}" && 7z x -aoa "${f}"; rm -f "${f}"'
  run_extract mdf.xz 'f=`mktemp /tmp/extract.mdf.xz.XXXXXX`; xz -d -c "$src" > "${f}" && 7z x -aoa "${f}"; rm -f "${f}"'
  run_extract mdf.Z 'f=`mktemp /tmp/extract.mdf.Z.XXXXXX`; uncompress -c "$src" > "${f}" && 7z x -aoa "${f}"; rm -f "${f}"'
  run_extract mdf '7z x -aoa "$src"'

  NEW_COUNT=$(find -L . -type d | wc -l)
  [[ $NEW_COUNT -eq $COUNT ]] && break
  COUNT=$NEW_COUNT
done

chmod -R 755 . >/dev/null 2>&1
chown -R ${USER}:${GROUP} . >/dev/null 2>&1

# ancient (single file)
for i in {1..2}; do
  echo "Extracting ancient ($i/2) ..."
  find -L . -type f -print0 | while IFS= read -r -d $'\0' file
  do
    ancient i "$file" 2>&1 || true
  done \
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
