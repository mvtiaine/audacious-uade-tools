# Tools

## Audio Matching

Identify Amiga exotic modules and tracker music from audio files or via microphone.

The tool uses simple brute force approach for chroma similarity matching. On M4 Max it takes about 4-10 seconds, depending on input length. All CPU cores are utilized.

Proper implementation should use something like https://github.com/acoustid/acoustid-index or https://github.com/acoustid/pg_acoustid

It's recommended to record at least 30s of audio, but the more the better. Accuracy can depend on many factors, like audio quality and unique audio features available. For best results use `fpcalc`and `audio_match.sc` directly with chromaprint generated from the original audio file (like YouTube rip), instead of using microphone. Also make sure the recording only consists of (part of) the actual music to be matched, no random noises, silence etc. in beginning or end.

## Dupe Finder

Find dupes of the given music file (e.g. (non-)original, corrupted or modified versions) in various sources, based on audio fingerprints.
It requires that the file MD5 exists in the database, if not you should use the audio matching tool instead.
On M4 Max it takes about 2 seconds to run. All CPU cores are utilized.

## Source Metas

Print metadatas for files in specific source or directory, or detect unique files/md5s not found in songdb or other sources. Does not use/require audio fingerprint files.


## Usage

See [Docker setup](#docker-setup) for docker support.

**Requirements:** scala-cli (https://scala-cli.virtuslab.org/), zstd, 8GB+ of memory. For audio matching: chromaprint (fpcalc). For microphone support: SoX, (macOS) mic permission for terminal. Also make sure mic input volume is high enough.

**Setup:**

Download and decompress audio fingerprint files:

```bash
mkdir -p songdb/sources/audio
cd songdb/sources/audio
rm -f audio_*.zst
for i in 0 1 2 3 4 5 6 7 8 9 a b c d e f; do wget https://github.com/mvtiaine/audacious-uade-tools/releases/download/audio/audio_$i.tsv.zst; done
zstd -d -f --rm audio_*.zst
```

Fetch dependencies:

```bash
cd songdb
./audio_match.sc
./find_dupes.sc
./source_metas.sc
```

**Usage:**

Note: the default "human readable" output may need very wide terminal (240 chars), and still truncate the output. Use option `--tsv` to output as TSV without truncation.

```bash
./audio_match.sc                                 # Prints usage
./audio_match.sc AQAAC1EShUokRcMfoT-OX8RfNKH...  # Match specific chromaprint
fpcalc -plain somefile.wav | ./audio_match.sc -  # Calculate and match chromaprint from audiofile
./record.sh                                      # Prints usage
./record.sh 0                                    # Interactive recording and matching using microphone
./record.sh 30                                   # Record and match 30 seconds using microphone
./find_dupes.sc                                  # Prints usage
./find_dupes.sc somefile.mod                     # Finds dupes in database for somefile.mod
./find_dupes.sc --all --tsv somefile.mod         # Print path for each dupe in separate line and use TSV output
./source_metas.sc                                # Prints usage
./source_metas.sc ~/mods                         # Show metadata for files in ~/mods
./source_metas.sc --unique --tsv Deck > deck.tsv # Show unique MD5s in Deck module collection as TSV

# Use audacious-uade CLI player to play an unknown file and match the chromaprint
PROBE=1 ~/audacious-uade/src/plugin/cli/player/player 11025 ~/somefile.mod | sox -t raw -b 16 -e signed -c 2 -r 11025 - -t raw -b 16 -e signed -c 1 -r 11025 -D - remix 1-2 | fpcalc -length 9999 -rate 11025 -channels 1 -format s16le -plain - | ./audio_match.sc -

# or with uade123
uade123 -1 -p 1 --filter=NONE --resampler=none -e raw --frequency=11025 -c ~/somefile.mod | sox -t raw -b 16 -e signed -c 2 -r 11025 - -t raw -b 16 -e signed -c 1 -r 11025 -D - remix 1-2 | fpcalc -length 9999 -rate 11025 -channels 1 -format s16le -plain - | ./audio_match.sc -

# or with xmp
xmp -i nearest -f 11025 -c ~/somefile.mod | sox -t raw -b 16 -e signed -c 2 -r 11025 - -t raw -b 16 -e signed -c 1 -r 11025 -D - remix 1-2 | fpcalc -length 9999 -rate 11025 -channels 1 -format s16le -plain - | ./audio_match.sc -
```

See `songdb/audio_match.sc`, `songdb/record.sh`, `songdb/find_dupes.sc` and `songdb/source_metas.sc` sources for more details.

**Note:**: audio TSV files and git repo must be in sync

**Note:**: Run `./audio_match.sc` once before running `./record.sh`. It will fetch the Scala dependencies on first run, which takes a while.

**Note:**: Only tested on macOS and Linux.

**Example output:**

Note that output depends on tool used, for `audio_match.sc` default output looks like:

```
Score | MD5          | Size    | Format                      | Player     | Sub | Len   | Ch | Filenames                      | #  | Authors     | Album                 | Publishers                 | Year
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
0,943 | fb778dace14a | 71206   | Protracker                  | uade       | 1   | 03:58 |    |                                | 1  | Interphace  | The Co-Operation Demo | Andromeda & Infernal Minds | 1990
0,943 | cb41fba3043b | 71206   | Protracker                  | uade       | 1   | 03:58 |    | mod.dawn                       | 2  | Interphace  | The Co-Operation Demo | Andromeda & Infernal Minds | 1990
0,943 | 36a8a32a0314 | 71206   | Protracker                  | uade       | 1   | 03:58 |    |                                | 1  | Interphace  | The Co-Operation Demo | Andromeda & Infernal Minds | 1990
0,943 | 0489859f3ad9 | 52680   | Digital Symphony            | libopenmpt | 0   | 03:57 | 4  | DAWN                           | 1  |             |                       |                            |     
0,940 | bf2ce1133d7a | 71206   | Soundtracker II (31 instr.) | uade       | 1   | 03:58 |    | mod.music1                     | 1  | Interphace  | The Co-Operation Demo | Andromeda & Infernal Minds | 1990
```

List of top matched entries with match score, MD5, subsong and some metadata from songdb (# == number of sources where MD5 is found).
Use the `--tsv` option to get non-truncated output.

You can also grep the MD5s from TSVs to locate the matching files in sources and all available metadata:

```bash
grep MD5 sources/[b-z]*/*.tsv
grep MD5 ../tsv/pretty/md5/*.tsv
```

## Docker setup

```bash
cd songdb
# Build image:
docker build . -t audacious-uade-tools
# Build image (avoid cache):
docker build . --no-cache -t audacious-uade-tools

# Example usages:

# - Match specific chromaprint
docker run --rm audacious-uade-tools ./audio_match.sc AQAAC1EShUokRcMfoT-OX8RfNKHCG5V6iEue48cdHQAEEgYRCQhA0AAD

# - Calculate and match chromaprint from audiofile
cat somefile.wav | docker run -i --rm audacious-uade-tools /bin/bash -c "fpcalc -plain - | ./audio_match.sc -"

# - Record and match using microphone:
# (ctrl-c to stop recording, record at least 30+s, SoX is still needed on the host for rec command)
rec -r 11025 -c 1 -t wav - | docker run -i --rm audacious-uade-tools /bin/bash -c "fpcalc -plain - | ./audio_match.sc -"

# - Finds dupes in database
cat somefile.mod | docker run -i --rm audacious-uade-tools ./find_dupes.sc -

# - Unique MD5s for Funet source as TSV
docker run -i --rm audacious-uade-tools ./source_metas.sc --unique --tsv Funet > funet_unique.tsv
```
