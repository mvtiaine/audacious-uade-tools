# audacious-uade-tools

This repo contains the songdb TSV files, and Scala CLI scripts for generating them, used by [audacious-uade](https://github.com/mvtiaine/audacious-uade) and [other projects](#used-by).

The database contains songlengths and module infos for almost 480000 unique MD5s, and metadata (authors/album/publishers/year) for 380000, processed from around 400 [sources](sources.md).

An experimental Shazam-like tool is also included for identifying music from audio files or via microphone (see [Audio Matching](tools.md#audio-matching)).

And another tool to help finding original versions of music files, among modified or corrupted versions (see [Dupe Finder](tools.md#dupe-finder)).

There's also a tool to show metadatas for files in specific source or directory, or detect unique files/md5s (see [Source Metas](tools.md#source-metas)).


## Directories

- **songdb/** - Scala CLI, SQL scripts and raw source TSVs to generate the final processed TSV files, and related tools.
- **tsv/encoded/** - the songdb TSV files used by audacious-uade. The files are "encoded" to almost binary format to optimize for size and fast in-memory songdb initialization.
- **tsv/pretty/** - pretty printed / clear text versions of the TSV files. See [TSV Format Specification](songdb.md#tsv-format-specification).
- **misc/** - misc bash scripts


## Songdb

See [songdb.md](songdb.md) for the database usage instructions.


## Tools

See [tools.md](tools.md) for tool usage instructions.


## License

The Scala and SQL scripts are licensed under **GPL-2.0-or-later**.

For any applicable sui generis rights or copyrights I may have over the database files, they are provided under **CC BY-NC-SA 4.0** license.

### LLM usage

Parts of the codebase have been edited with help of various LLMs. While the legal and ethical issues are unresolved, I consider those edits public domain since the training material was treated as such anyway.
So any files that include `SPDX-AI-Disclosure: ai-assisted` or `ai-generated` tags are also available under **CC-PDM-1.0** at your discretion. Any third party code, modified or unmodified, retain their original copyright and license.
FWIW I have not paid a cent to any company for the LLM usage.

### Sources

See [sources.md](sources.md) for sources used for the database.


## Used By

This database is also used by:

- **16bit player** - https://nexus0.net/pub/sw/16bitplayer/
- **DEViLBOX** - https://devilbox.uprough.net/
- **HippoPlayer** - https://github.com/koobo/HippoPlayer
- **LMS Game Music / Tracker MOD/MIDI Player** - https://nexus0.net/pub/sw/lmsmodplay/
- **Modizer** - https://github.com/yoyofr/modizer
- **Protracktor** - https://github.com/przunk/protracktor
- **rewamp** - https://rewamp.app/
- **SoniqBoom** - https://github.com/SFCyris/SoniqBoom


## Contact

My email address is [firstname].[lastname][at]aalto.fi

The old address mvtiaine@cc.hut.fi no longer works.
