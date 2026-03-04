:warning: Repository history was recently filtered with git-filter-repo to reduce size, so recloning is needed. Also audio fingerprint TSVs are now separate downloads. Sorry for any inconvenience.

# audacious-uade-tools

This repo contains Scala CLI scripts for generating songdb TSV files used by [audacious-uade](https://github.com/mvtiaine/audacious-uade).

Also an experimental Shazam like tool is included (see [Audio Matching](#audio-matching)) for identifying music from audio files or via microphone.


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
- `tsv/pretty/*/modland.tsv` - author/album metadata sourced from Modland
- `tsv/pretty/*/modsanthology.tsv` - author/publisher/album/year metadata sourced from Mods Anthology
- `tsv/pretty/*/oldexotica.tsv` - author/publisher/album/year metadata sourced from ExoticA (old)
- `tsv/pretty/*/unexotica.tsv` - author/publisher/album/year metadata sourced from UnExoticA
- `tsv/pretty/*/wantedteam.tsv` - author/publisher/album/year metadata sourced from Wanted Team

## Raw TSV Source Files

- `songdb/sources/*.tsv` - module infos and songlengths for each site/source
- `songdb/sources/demozoo_leftovers.tsv` - module infos and songlengths for Demozoo downloads (excluding duplicated sources). Link list is generated with `songdb/scripts/sql/demozoo_leftovers.sql`
- `songdb/sources/demozoo_music.tsv` - Demozoo metadata generated with SQL query (`songdb/scripts/sql/demozoo_music.sql`) from Demozoo postgres database dump
- `songdb/sources/demozoo_prods.tsv` - Demozoo metadata generated with SQL query (`songdb/scripts/sql/demozoo_prods.sql`) from Demozoo postgres database dump
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


## Audio Matching

Identify Amiga exotic modules and tracker music from audio files or via microphone.

The tool uses simple brute force approach for chroma similarity matching, so can be a bit slow. On M4 Max it takes from half a minute to a minute or two depending on input length. All CPU cores are utilized.

Proper implementation should use something like https://github.com/acoustid/acoustid-index or https://github.com/acoustid/pg_acoustid

It's recommended to record at least 30s of audio, but the more the better. Accuracy can depend on many factors, like audio quality and unique audio features available. For best results use `fpcalc`and `audio_match.sc` directly with chromaprint generated from the original audio file (like YouTube rip), instead of using microphone.

**Requirements:** scala-cli (https://scala-cli.virtuslab.org/), chromaprint (fpcalc), 8GB+ of memory. For microphone support: sox, (macOS) mic permission for terminal. Also make sure mic input volume is high enough.

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
```

**Usage:**

```bash
./audio_match.sc                                 # Prints usage
./audio_match.sc AQAAC1EShUokRcMfoT-OX8RfNKH...  # Match specific chromaprint
fpcalc -plain somefile.wav | ./audio_match.sc -  # Calculate and match chromaprint from audiofile
./record.sh                                      # Prints usage
./record.sh 0                                    # Interactive recording and matching using microphone
./record.sh 30                                   # Record and match 30 seconds using microphone
```

See `songdb/audio_match.sc` and `songdb/record.sh` sources for more details.

**Note:**: audio TSV files and git repo must be in sync

**Note:**: Run `./audio_match.sc` once before running `./record.sh`. It will fetch the Scala dependencies on first run, which takes a while.

**Note:**: Only tested on macOS and Linux.

**Output:**

```
Score | MD5          | Sub | Authors    | Album                 | Publishers                 | Year | Filenames             
----------------------------------------------------------------------------------------------------------------------------
0,935 | 98d24339316c | 1   | Interphace | The Co-Operation Demo | Andromeda & Infernal Minds | 1990 | MOD.dawn, dawn.mod    
0,787 | a241710e5f1f | 1   |            |                       |                            |      | lords of the boards.xm
0,781 | d3a158c9db44 | 0   | Slammy     |                       |                            |      | final voyage.it 
```

List of top matched entries with match score, MD5, subsong and some metadata from songdb.
You can grep the MD5 from `songdb/sources/*/*.tsv` and `tsv/pretty/md5/*.tsv` to locate the matching mod file and all available metadata.


## License

The Scala and SQL scripts are licensed under **GPL-2.0-or-later**.

For any applicable sui generis rights or copyrights I may have over the database files, they are provided under **CC BY-NC-SA 4.0** license.


### Sources

Sources used for the database:

- **Amiga Music Preservation (AMP)** - https://amp.dascene.net/
- **AmigaMega** - https://amigamega.com/
- **Aminet** - https://aminet.net/
- **Classic Game Soundtracks** - https://www.nemmelheim.de/cgs_small/index2.php
- **Demozoo** - https://demozoo.org/
- **eXoDOS** - https://www.retro-exo.com/
- **ExoticA (old)** - http://old.exotica.org.uk/
- **Fujiology** - https://fujiology.untergrund.net/
- **Lemon Amiga Ultimate MOD pack!** - https://www.lemonamiga.com/forum/viewtopic.php?t=14863
- **MBnet Apaja** - https://archive.org/details/modit
- **Modland** - http://ftp.modland.com/
- **Mods Anthology** - https://archive.org/details/cdrom-amiga-mods-anthology-1
- **NostalgicPlayer** - https://nostalgicplayer.dk/
- **Protracker Modules Gpack** - https://bsky.app/profile/nogorg.bsky.social/post/3mby77chfss2b
- **Stone Oakvalley's Amiga Music Collection (SOAMC=)** - https://www.paula8364.com
- **The Mod Archive** - https://modarchive.org/
- **TOSEC** - https://www.tosecdev.org/
- **TOSEC Music** - https://www.nzbking.com/details:5191a2da123c8c751b3ffcbb/
- **UnExoticA** - https://www.exotica.org.uk/wiki/UnExoticA
- **Wanted Team** - http://wt.exotica.org.uk/
- **WHDLoad-Database** - https://github.com/MrV2K/WHDLoad-Database
- **Wikipedia** - https://wikipedia.org
- **Zakalwe** - git://zakalwe.fi/chip

And my old Amiga mod collection.


## Used By

This database is also used by:

- **HippoPlayer** - https://github.com/koobo/HippoPlayer

## Contact

My email address is [firstname].[lastname][at]aalto.fi

The old address mvtiaine@cc.hut.fi no longer works.
