// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.util.Using

enum Source:
  case
    Modland,
    AMP,
    UnExotica,
    Mods_Anthology,
    Wanted_Team,
    Zakalwe,
    Aminet,
    Modland_Incoming,
    Demozoo_Leftovers,
    OldExotica,
    ModArchive,
    TOSEC_Music,
    TOSEC_Music_Unknown,
    SOAMC,
    Fujiology,
    MBnet,
    NostalgicPlayer

import Source._

val tsvfiles = Buffer(
  ("modland.tsv", Modland),
  ("amp.tsv", AMP),
  ("unexotica.tsv", UnExotica),
  ("modsanthology.tsv", Mods_Anthology),
  ("wantedteam.tsv", Wanted_Team),
  ("zakalwe.tsv", Zakalwe),
  ("aminet.tsv", Aminet),
  ("modland_incoming.tsv", Modland_Incoming),
  ("demozoo_leftovers.tsv", Demozoo_Leftovers),
  ("oldexotica.tsv", OldExotica),
  ("modarchive.tsv", ModArchive),
  ("tosecmusic.tsv", TOSEC_Music),
  ("tosecmusic_unknown.tsv", TOSEC_Music_Unknown),
  ("soamc.tsv", SOAMC),
  ("fujiology.tsv", Fujiology),
  ("mbnet.tsv", MBnet),
  ("nostalgicplayer.tsv", NostalgicPlayer),
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
  path: String,
)

lazy val tsvs = tsvfiles.par.map(tsv => (tsv._2, Using(scala.io.Source.fromFile(s"sources/${tsv._1}")(using scala.io.Codec.ISO8859))(_.getLines.map(line =>
  val l = line.split("\t")
  if (l.length > 4) TsvEntry(l(0), l(1).toInt, l(2).toInt, l(3), l(4), l(5), if (l(6).isEmpty) 0 else l(6).toInt, l(7).toInt, l(8), l(9))
  else TsvEntry(l(0), l(1).toInt, l(2).toInt, l(3), "", "", 0, -1, "", "")
).toBuffer).get.sortBy(e => (e.md5, e.subsong)).groupBy(_.md5))).seq

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
}

lazy val modland = readSourceDB(Modland)
lazy val amp = readSourceDB(AMP)
lazy val unexotica = readSourceDB(UnExotica)
lazy val modsanthology = readSourceDB(Mods_Anthology)
lazy val wantedteam = readSourceDB(Wanted_Team)
lazy val zakalwe = readSourceDB(Zakalwe)
lazy val aminet = readSourceDB(Aminet)
lazy val modland_incoming = readSourceDB(Modland_Incoming)
lazy val demozoo_leftovers = readSourceDB(Demozoo_Leftovers)
lazy val oldexotica = readSourceDB(OldExotica)
lazy val modarchive = readSourceDB(ModArchive)
lazy val tosecmusic = readSourceDB(TOSEC_Music)
lazy val tosecmusic_unknown = readSourceDB(TOSEC_Music_Unknown)
lazy val soamc = readSourceDB(SOAMC)
lazy val fujiology = readSourceDB(Fujiology)
lazy val mbnet = readSourceDB(MBnet)
lazy val nostalgicplayer = readSourceDB(NostalgicPlayer)
