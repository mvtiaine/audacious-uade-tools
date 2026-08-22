// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025-2026 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.Using
import scala.util.boundary, boundary.break

import convert._
import sources.Source
import sources.SourceDBEntry

final case class WHDLoadMeta(
  fullName: String,
  shortName: String,
  _type: String,
  hardware: String,
  genre: String,
  producer: String,
  developer: String,
  releaseParty: String,
  year: Int,
  language: String,
  players: Int,
  chipset: String,
  tvSystem: String,
  audio: String,
  retroPlayUrl: String
)

val whdloaddb_csv = System.getProperty("user.home") + "/sources/metadata/whdload/WHDLoad_Database.csv"

lazy val metas = Using(scala.io.Source.fromFile(whdloaddb_csv)(using scala.io.Codec.ISO8859))(_.getLines().toBuffer.par.map { line =>
  val l = line.split(";")
  var meta = WHDLoadMeta(
    fullName = l(0).trim,
    shortName = l(1).trim,
    _type = l(2).trim,
    hardware = l(3).trim,
    genre = l(4).trim,
    producer = l(5).trim,
    developer = l(6).trim,
    releaseParty = l(7).trim,
    year = l(8).toIntOption.getOrElse(0),
    language = l(9).trim,
    players = l(10).toIntOption.getOrElse(0),
    chipset = l(11).trim,
    tvSystem = l(12).trim,
    audio = l(13).trim,
    retroPlayUrl = l(31).trim
  )
  // XXX quirks
  if (meta.shortName == "Book Of Songs" && meta.year == 1992) meta = meta.copy(year = 1993)
  else if (meta.shortName.startsWith("James Pond 2") && meta.year == 1993) meta = meta.copy(year = 1991)
  else if (meta.shortName == "Explora 2" && meta.year == 1988) meta = meta.copy(year = 1989)
  else if (meta.shortName == "Dizzy Tunes 2" && meta.year == 1994) meta = meta.copy(year = 1993)
  else if (meta.shortName.startsWith("Super SkidMarks ") && meta.year == 1998) meta = meta.copy(year = 1995)
  else if (meta.shortName == "Deluxe Galaga" && meta.year == 1995) meta = meta.copy(year = 1993)
  else if (meta.shortName == "Book of Songs" && meta.year == 1992) meta = meta.copy(year = 1993)
  meta
}).get

lazy val articlePattern = """^(.*), (The|A|An|Das|Der|Die|Les|Le|La|El)\b(.*)""".r

private def normalize(s: String): String = {
  // Remove any text inside parentheses
  val withoutParens = s.replaceAll("""\([^)]*\)""", "").trim
  // Move trailing article to front
  articlePattern.findFirstMatchIn(withoutParens) match {
    case Some(m) => s"${m.group(2)} ${m.group(1)}${m.group(3)}".trim
    case None => withoutParens
  }
}

lazy val whdloadMetas = metas.par.map(m =>
  val publishers = Buffer(m.producer, m.developer).map(normalize).flatMap(p =>
    p.split("  ").flatMap(p =>
      if (!Set("Hobby & Work","K & A Plus","Mahoney & Kaktus","Manley & Associates").contains(p)) p.split(" & ")
      else Array(p))
  ).map(_.trim).filter(_.nonEmpty).distinct.sorted
  val _type =
    if (m._type == "Beta")
      if (m.genre == "Demo") "Demo" else "Game"
    else m._type
  val meta = MetaData(
    hash = "",
    authors = Buffer.empty,
    album = normalize(m.fullName).trim,
    publishers = publishers.sorted.distinct.toBuffer,
    year = m.year,
    _type = _type.trim,
    _platform = "Amiga",
  )
  meta
).toSet

val retroplay_by_path = sources.sourceDB(Source.RetroPlayWHDLoadPacks).groupBy(_.path.toLowerCase).to(TreeMap)

val whdloadExtras = metas.par.map(meta =>
  val path = meta.retroPlayUrl
    .replace("https://ftp2.grandis.nu/turran/FTP/Retroplay%20WHDLoad%20Packs/","")
    .replace("Beta_&_Unreleased/", "Beta_&_Unofficial/")
    .toLowerCase

  val md5s = sources.findArchive(path, retroplay_by_path).map(_._1).sorted.distinct
  md5s.distinct.map((_, (meta, md5s.distinct)))
).flatten.seq.groupBy(_._1).mapValues(_.map(_._2)).par.flatMap { case (md5, _metas) =>
  val minyear = _metas.map(m => if (m._1.year > 0) m._1.year else 9999).min
  val metas = _metas.filter(_._1.year <= minyear).map { case (meta, _) =>
    val publishers = Buffer(meta.producer, meta.developer).map(normalize).flatMap(p =>
      p.split("  ").flatMap(p =>
        if (!Set("Hobby & Work","K & A Plus","Mahoney & Kaktus","Manley & Associates").contains(p)) p.split(" & ")
        else Array(p))
    ).map(_.trim).filter(_.nonEmpty).distinct.sorted
    val _type =
      if (meta._type == "Beta")
        if (meta.genre == "Demo") "Demo" else "Game"
      else meta._type
    val album = normalize(meta.fullName)
    (meta.retroPlayUrl
      .replace("https://ftp2.grandis.nu/turran/FTP/Retroplay%20WHDLoad%20Packs/","")
      .replace("Beta_&_Unreleased/", "Beta_&_Unofficial/")
    , MetaData(
      hash = md5.take(12),
      authors = Buffer.empty,
      album = album,
      publishers = publishers.sorted.distinct.toBuffer,
      year = meta.year,
      _type = _type.trim,
      _platform = "Amiga",
    ))
  }
  if (metas.isEmpty) {
    None
  } else {
    Some(metas.sortBy(m => (if (m._2._type.toLowerCase == "game") 0 else 1000) + m._1.length).head)
  }
}.seq.toBuffer.distinct
