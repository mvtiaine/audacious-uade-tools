// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.util.Using

enum Source:
  case
    AmigaMega_Demos,
    AmigaMega_Games,
    Aminet,
    AMP,
    Classic_Game_Soundtracks,
    Demozoo_Leftovers,
    Fujiology,
    Lemon_Amiga,
    MBnet,
    ModArchive,
    Modland,
    Modland_Incoming,
    Mods_Anthology,
    NostalgicPlayer,
    OldExotica,
    ProTracker_Modules_GPack,
    SOAMC,
    TOSEC_Music,
    TOSEC_Music_Unknown,
    Tundrah,
    UnExotica,
    Wanted_Team,
    Zakalwe

import Source._

val tsvfiles = Buffer(
  ("bbs/mbnet.tsv", MBnet),
  ("cd/modsanthology.tsv", Mods_Anthology),
  ("collection/lemonamiga.tsv", Lemon_Amiga),
  ("collection/protrackermodulesgpack.tsv", ProTracker_Modules_GPack),
  ("collection/tosecmusic.tsv", TOSEC_Music),
  ("collection/tosecmusic_unknown.tsv", TOSEC_Music_Unknown),
  ("collection/tundrah.tsv", Tundrah),
  ("site/amigamega_demos.tsv", AmigaMega_Demos),
  ("site/amigamega_games.tsv", AmigaMega_Games),
  ("site/aminet.tsv", Aminet),
  ("site/amp.tsv", AMP),
  ("site/classicgamesoundtracks.tsv", Classic_Game_Soundtracks),
  ("site/demozoo_leftovers.tsv", Demozoo_Leftovers),
  ("site/fujiology.tsv", Fujiology),
  ("site/modarchive.tsv", ModArchive),
  ("site/modland.tsv", Modland),
  ("site/modland_incoming.tsv", Modland_Incoming),
  ("site/nostalgicplayer.tsv", NostalgicPlayer),
  ("site/oldexotica.tsv", OldExotica),
  ("site/soamc.tsv", SOAMC),
  ("site/unexotica.tsv", UnExotica),
  ("site/wantedteam.tsv", Wanted_Team),
  ("site/zakalwe.tsv", Zakalwe),
);

case class TsvEntry (
  md5: String,
  subsong: Int,
  songlength: Int,
  songend: String,
  player: String,
  format: String,
  channels: Int,
  filesize: Int,
  xxh32: String,
  crc32: String,
  path: String,
)

lazy val tsvs = tsvfiles.par.map(tsv => (tsv._2, Using(scala.io.Source.fromFile(s"sources/${tsv._1}")(using scala.io.Codec.ISO8859))(tsv =>
  var player = ""
  tsv.getLines.map(line =>
    val l = line.split("\t")
    if (l.length > 4) {
      player = l(4)
      TsvEntry(l(0), l(1).toInt, l(2).toInt, l(3), player, l(5), if (l(6).isEmpty) 0 else l(6).toInt, l(7).toInt, l(8), l(9), l(10))
    } else TsvEntry(l(0), l(1).toInt, l(2).toInt, l(3), player, "", 0, -1, "", "", "")
  ).toBuffer
).get.sortBy(e => (e.md5, e.subsong)).groupBy(_.md5))).seq

case class SourceDBEntry (
  md5: String,
  path: String,
  filesize: Int,
  xxh32: String,
)

def readSourceDB(source: Source) = {
  tsvs.filter(_._1 == source).par.flatMap(_._2).map({case (md5,subsongs) =>
    if (subsongs.groupBy(_.subsong).exists(_._2.size > 1)) {
      System.err.println("INFO: duplicate files in " + source + " for " + md5 + ": " + subsongs)
    }
    subsongs.filter(_.path != "").map(e =>
      SourceDBEntry(md5, e.path, e.filesize, e.xxh32)
    )
  }).flatten.seq
}.toSeq

lazy val aminet = readSourceDB(Aminet)
lazy val amp = readSourceDB(AMP)
lazy val demozoo_leftovers = readSourceDB(Demozoo_Leftovers)
lazy val fujiology = readSourceDB(Fujiology)
lazy val modland = readSourceDB(Modland)
lazy val modsanthology = readSourceDB(Mods_Anthology)
lazy val oldexotica = readSourceDB(OldExotica)
lazy val tosecmusic = readSourceDB(TOSEC_Music)
lazy val tosecmusic_unknown = readSourceDB(TOSEC_Music_Unknown)
lazy val unexotica = readSourceDB(UnExotica)
lazy val wantedteam = readSourceDB(Wanted_Team)
