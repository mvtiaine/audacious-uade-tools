# Songdb

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
- `songdb/sources/strings/*/*.strings` - strings extracted from modules, separate download.

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

**Note:** I reserve the right to change the format or location in Github of any of the TSV or other files at any time. It's strongly recommended to link to a specific git revision for download urls, git submodule references etc. to avoid surprises.
