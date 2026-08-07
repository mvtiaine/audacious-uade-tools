# audacious-uade-tools

This repo contains Scala CLI scripts for generating songdb TSV files used by [audacious-uade](https://github.com/mvtiaine/audacious-uade).

The database contains songlengths and module infos for almost 480000 unique MD5s, and metadata (authors/album/publishers/year) for 380000, processed from around 400 [sources](sources.md).

An experimental Shazam-like tool is also included for identifying music from audio files or via microphone (see [Audio Matching](#audio-matching)).

And another tool to help finding original versions of music files, among modified or corrupted versions (see [Dupe Finder](#dupe-finder)).


## Directories

- **songdb/** - Scala CLI, SQL scripts and raw source TSVs to generate the final processed TSV files
- **tsv/encoded/** - the songdb TSV files used by audacious-uade. The files are "encoded" to almost binary format to optimize for size and fast in-memory songdb initialization.
- **tsv/pretty/** - pretty printed / clear text versions of the TSV files. See [TSV Format Specification](#tsv-format-specification).
- **misc/** - misc bash scripts


## Hashing

There are two alternative hashing methods provided and separate TSVs for each under md5 and xxh32 subfolders.
Hashes are calculated from decompressed files, even if the original source files were compressed.

- **MD5** - 48-bits (MSB) as hex, hash calculated from whole file
- **XXH32+filesize** - 48-bits as hex (32-bit + 16-bit). Calculated+concatenated as hex(XXH32(file)) + hex(filesize & 0xFFFF). XXH32 is calculated from max first 256k bytes only, filesize is full filesize.


## Songdb TSV Files

- `tsv/pretty/*/songlengths.tsv` - subsong and songlengths info
- `tsv/pretty/*/modinfos.tsv` - module file format and channel info
- `tsv/pretty/*/metadata.tsv` - all metadata from different sources distilled to single TSV

### Extra TSV Files

- `tsv/pretty/*/amp.tsv` - author/album metadata sourced from AMP
- `tsv/pretty/*/demozoo.tsv` - author/publisher/album/year metadata sourced from Demozoo
- `tsv/pretty/*/fujiology.tsv` - author/publisher/album/year metadata sourced from Fujiology
- `tsv/pretty/*/kestra.tsv` - author/publisher/album/year metadata sourced from Kestra / Bitworld
- `tsv/pretty/*/modland.tsv` - author/album metadata sourced from Modland
- `tsv/pretty/*/modsanthology.tsv` - author/publisher/album/year metadata sourced from Mods Anthology
- `tsv/pretty/*/oldexotica.tsv` - author/publisher/album/year metadata sourced from ExoticA (old)
- `tsv/pretty/*/unexotica.tsv` - author/publisher/album/year metadata sourced from UnExoticA
- `tsv/pretty/*/wantedteam.tsv` - author/publisher/album/year metadata sourced from Wanted Team

## Raw TSV Source Files

- `songdb/sources/*/*.tsv` - module infos and songlengths for each site/source
- `songdb/sources/metadata/demozoo_*.tsv` - Demozoo metadata generated with SQL queries in (`songdb/scripts/sql/demozoo_*.sql`) from Demozoo postgres database dump
- `songdb/sources/audio/*.tsv` - audio fingerprints (chromaprint), separate download. See `scripts/sources/audio.sc` for format.

The module infos and songlength TSVs are generated using the precalc binary+script from [audacious-uade](https://github.com/mvtiaine/audacious-uade/blob/master/src/plugin/cli/precalc/) from my local copy/mirror/snapshot of the various sites/sources.

**Note:** Audio fingerprint files must be separately downloaded from https://github.com/mvtiaine/audacious-uade-tools/releases/tag/audio
See [Audio Matching](#audio-matching) for setup.

**Note:** Some additional required files not included in Github, specifically local mirror of some of source web pages and/or database files are needed to actually run the Scala `songdb.sc` script.

**Note:** Only files playable by audacious-uade are included in the database. The script runs completely locally and does not download anything from internet.


## TSV Format Specification

Here are example snippets and short spec for the pretty printed TSVs. Example parsing code can be found in `songdb/scripts/pretty.sc`

### songlengths.tsv

```
ff5c7b3227e0	0	65920,p 65920,p,!
fffd7a7d8547	1	250840,p+s
fffdc1d765c3	0	40880,l 117860,l 8780,s 79340,l 8080,s 19000,s
```

Format: `[hash]<TAB>[minsubsong]<TAB>[[songlength(ms),songend[,!]]<SPACE>[songlength(ms),songend[,!]]<SPACE>[...]]`

- Duplicate subsongs are denoted by `!`

### modinfos.tsv

```
fffdc1d765c3	CustomPlay	
fffdd3c2bef3	Scream Tracker 3.2x (GUS)	8
fffe869a7f8d	AHX v2	
```

Format: `[hash]<TAB>[format]<TAB>[channels]`

### metadata.tsv

```
feaa9d2a4869	Scorpik	Alchemy	Toxic Ziemniak	1992
feaba2f4c992	Jazz			
feabaabf8a62	Mantronix~Tip	Blue House Productions~Rebels~Sonic Projects	Blue House 2	1991
```

Format: `[hash]<TAB>[authors]<TAB>[publishers]<TAB>[album]<TAB>[year]`

- Multiple authors or publishers are separated by `~`

The TSV files use UTF-8 encoding.

**Note:** I reserve the right to change the format or location in Github of any of the TSV or other files at any time.


## Tools

### Audio Matching

Identify Amiga exotic modules and tracker music from audio files or via microphone.

The tool uses simple brute force approach for chroma similarity matching. On M4 Max it takes about 5-10 seconds, depending on input length. All CPU cores are utilized.

Proper implementation should use something like https://github.com/acoustid/acoustid-index or https://github.com/acoustid/pg_acoustid

It's recommended to record at least 30s of audio, but the more the better. Accuracy can depend on many factors, like audio quality and unique audio features available. For best results use `fpcalc`and `audio_match.sc` directly with chromaprint generated from the original audio file (like YouTube rip), instead of using microphone.

### Dupe Finder

Find dupes of the given music file (e.g. (non-)original, corrupted or modified versions) in various sources, based on audio fingerprints.
It requires that the file MD5 exists in the database, if not you should use the audio matching tool instead.
On M4 Max it takes 2-3 seconds to run. All CPU cores are utilized.

### Usage

**Requirements:** scala-cli (https://scala-cli.virtuslab.org/), 8GB+ of memory. For audio matching: chromaprint (fpcalc). For microphone support: sox, (macOS) mic permission for terminal. Also make sure mic input volume is high enough.

**Setup:**

Download and decompress audio fingerprint files:

```bash
mkdir -p songdb/sources/audio
cd songdb/sources/audio
rm audio_*.zst
for i in {0..9} {a..f}; do wget https://github.com/mvtiaine/audacious-uade-tools/releases/download/audio/audio_$i.tsv.zst; done
zstd -d -f --rm audio_*.zst
```

Fetch dependencies:

```bash
cd songdb
./audio_match.sc
./find_dupes.sc
```

**Usage:**

```bash
./audio_match.sc                                 # Prints usage
./audio_match.sc AQAAC1EShUokRcMfoT-OX8RfNKH...  # Match specific chromaprint
fpcalc -plain somefile.wav | ./audio_match.sc -  # Calculate and match chromaprint from audiofile
./record.sh                                      # Prints usage
./record.sh 0                                    # Interactive recording and matching using microphone
./record.sh 30                                   # Record and match 30 seconds using microphone
./find_dupes.sc                                  # Prints usage
./find_dupes.sc somefile.mod                     # Finds dupes in database
```

See `songdb/audio_match.sc`, `songdb/record.sh` and `songdb/find_dupes.sc` sources for more details.

**Note:**: audio TSV files and git repo must be in sync

**Note:**: Run `./audio_match.sc` once before running `./record.sh`. It will fetch the Scala dependencies on first run, which takes a while.

**Note:**: Only tested on macOS and Linux.

**Output:**

```
Score | MD5          | Size  | Format                      | Sub | Filenames  | # | Authors    | Album                 | Publishers                 | Year
----------------------------------------------------------------------------------------------------------------------------------------------------------
0,943 | fb778dace14a | 71206 | Protracker                  | 1   |            | 1 | Interphace | The Co-Operation Demo | Andromeda & Infernal Minds | 1990
0,943 | cb41fba3043b | 71206 | Protracker                  | 1   | mod.dawn   | 2 | Interphace | The Co-Operation Demo | Andromeda & Infernal Minds | 1990
0,943 | 36a8a32a0314 | 71206 | Protracker                  | 1   |            | 1 | Interphace | The Co-Operation Demo | Andromeda & Infernal Minds | 1990
0,943 | 0489859f3ad9 | 52680 | Digital Symphony            | 0   | DAWN       | 1 |            |                       |                            |     
0,940 | bf2ce1133d7a | 71206 | Soundtracker II (31 instr.) | 1   | mod.music1 | 1 | Interphace | The Co-Operation Demo | Andromeda & Infernal Minds | 1990
```

List of top matched entries with match score, MD5, subsong and some metadata from songdb (# == number of sources where MD5 is found).
You can grep the MD5s from TSVs to locate the matching files in sources and all available metadata:

```bash
grep MD5 sources/[b-z]*/*.tsv
grep MD5 ../tsv/pretty/md5/*.tsv
```


## License

The Scala and SQL scripts are licensed under **GPL-2.0-or-later**.

For any applicable sui generis rights or copyrights I may have over the database files, they are provided under **CC BY-NC-SA 4.0** license.

### Sources

See [sources.md](sources.md) for sources used for the database.


## Used By

This database is also used by:

- **DEViLBOX** - https://devilbox.uprough.net/
- **HippoPlayer** - https://github.com/koobo/HippoPlayer
- **LMS Game Music / Tracker MOD/MIDI Player** - https://nexus0.net/pub/sw/lmsmodplay/
- **Modizer** - https://github.com/yoyofr/modizer
- **rewamp** - https://rewamp.app/


## Contact

My email address is [firstname].[lastname][at]aalto.fi

The old address mvtiaine@cc.hut.fi no longer works.
