// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2026 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

// TODO remove
//> using file ../convert.sc
//> using file ../dedup.sc
//> using file ../md5.sc
//> using file sources.sc

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.Using
import scala.util.boundary, boundary.break

import convert._
import sources.Source

// eXoID   MobyID  Game    File    Folder  Year    Publisher       Developer       Genre   Perspective     Misc    Series  URL     Collection      Notes   Compilation?    Foreign?        Add-On Included?
final case class eXoDOSMeta(
    //exoID: Int,
    //mobyID: Int,
    game: String,
    file: String,
    //folder: String,
    year: Int,
    publisher: String,
    developer: String,
    //genre: String,
    //perspective: String,
    //misc: String,
    //series: String,
    //url: String,
    //collection: String,
    //notes: String,
    //compilation: Boolean,
    //foreign: Boolean,
    //addonIncluded: Boolean,
)

val dosmaster_tsv = System.getProperty("user.home") + "/sources/exodos/DOS_Master - DOS_Master.tsv"

lazy val metas = Using(scala.io.Source.fromFile(dosmaster_tsv)(using scala.io.Codec.UTF8))(_.getLines().toBuffer.par.map { line =>
  val l = line.split("\\t")
  var year = l(5).trim.toIntOption.getOrElse(0)
  val file = l(3).trim
    // XXX quirks
  if (file == "Arcade Trivia Quiz (1989).zip") year = 1993
  else if (file == "Crazy Sue (1990).zip") year = 1991
  eXoDOSMeta(
    game = l(2).trim,
    file = file,
    year = year,
    publisher = l(6).trim,
    developer = l(7).trim,
  )
}).get

lazy val articlePattern1 = """^(.*), (The|A|An|El), (.*)""".r
lazy val articlePattern2 = """^(.*), (The|A|An|El)$""".r
lazy val articlePattern3 = """^(.*), (The|A|An|El) - (.*)""".r
lazy val articlePattern4 = """^(.*), (The|A|An|El) / (.*)""".r
lazy val articlePattern5 = """^(.*), (The|A|An|El): (.*)""".r

private def normalizeGame(s: String): String = {
  // Remove any text inside parentheses
  var normalized = s.trim.replaceAll("""\([^)]*\)""", "").trim
  // Move trailing article to front
  normalized = articlePattern1.findFirstMatchIn(normalized) match {
    case Some(m) => s"${m.group(2)} ${m.group(1)}, ${m.group(3)}".trim
    case None => normalized
  }
  normalized = articlePattern2.findFirstMatchIn(normalized) match {
    case Some(m) => s"${m.group(2)} ${m.group(1)}".trim
    case None => normalized
  }
  normalized = articlePattern3.findFirstMatchIn(normalized) match {
    case Some(m) => s"${m.group(2)} ${m.group(1)} - ${m.group(3)}".trim
    case None => normalized
  }
  normalized = articlePattern4.findFirstMatchIn(normalized) match {
    case Some(m) => s"${m.group(2)} ${m.group(1)} / ${m.group(3)}".trim
    case None => normalized
  }
    normalized = articlePattern5.findFirstMatchIn(normalized) match {
        case Some(m) => s"${m.group(2)} ${m.group(1)}: ${m.group(3)}".trim
        case None => normalized
    }
  normalized
}

private def normalize(s: String): Buffer[String] = {
  // Remove any text inside parentheses
  var normalized = normalizeGame(s)
 
  normalized = normalized
    .replaceAll(", Inc.,", ",")
    .replaceAll(", Inc,", ",")
    .replaceAll(" Co., LTD.", ",")
    .replaceAll(" Co., Ltd.,", ",")
    .replaceAll(" Pty., Ltd.,", ",")
    .replaceAll(", Pty. Ltd.,", ",")
    .replaceAll(", Pty Ltd.,", ",")
    .replaceAll(", Pty Ltd,", ",")
    .replaceAll(", Ltda.,", ",")
    .replaceAll(", Ltd.,", ",")
    .replaceAll(", Ltd,", ",")
    .replaceAll(", Pty.,", ",")
    .replaceAll(", Pty,", ",")
    .replaceAll(", S.A.,", ",")
    .replaceAll(", S.L.,", ",")
    .replaceAll(", Llc.,", ",")
    .replaceAll(", LLC,", ",")
    .replaceAll(", LLC.,", ",")
    .replaceAll(", Inc.$$", "")
    .replaceAll(", Inc$$", "")
    .replaceAll(" Pty., Ltd.$$", "")
    .replaceAll(", Ltd.$$", ",")
    .replaceAll(", Ltd$$", ",")
    .replaceAll(", Pty.$$", ",")
    .replaceAll(", Pty$$", ",")
    .replaceAll(", S.A.$$", ",")
    .replaceAll(", S.L.$$", ",")
    .replaceAll(", Llc.$$", ",")
    .replaceAll(", LLC$$", ",")
    .replaceAll(", LLC.$$", ",")
    .trim

  normalized.split(",").map(part =>
    val res = if (!Seq("The Learning Co.").contains(part.trim))
      part.trim
      .replaceAll(" Co., LTD.$$", "")
      .replaceAll(" Co., Ltd.$$", "")
      .replaceAll(" Pty. Ltd.$$", "")
      .replaceAll(" Pty Ltd$$", "")
      .replaceAll(" Inc.$$", "")
      .replaceAll(" Ltda.$$", "")
      .replaceAll(" Ltd.$$", "")
      .replaceAll(" Ltd$$", "")
      .replaceAll(" LTD$$", "")
      .replaceAll(" Plc$$", "")
      .replaceAll(" PLC$$", "")
      .replaceAll(" plc$$", "")
      .replaceAll(".SAS$$", "")
      .replaceAll(" S.A.S.$$", "")
      .replaceAll(" GesMBH$$", "")
      .replaceAll(" GmbH & Co. Produktions KG$$", "")
      .replaceAll(" GmbH & Co. KG$$", "")
      .replaceAll(" GmbH & Co.$$", "")
      .replaceAll(" GmbH$$", "")
      .replaceAll(" Pty$$", "")
      .replaceAll(" S.A.$$", "")
      .replaceAll(" S.L.$$", "")
      .replaceAll(" Llc.$$", "")
      .replaceAll(" LLC$$", "")
      .replaceAll(" LLC.$$", "")
      .replaceAll(" s.r.o.$$", "")
      .replaceAll(" S.R.L.$$", "")
      .replaceAll(" S.r.l.$$", "")
      .replaceAll(" s.r.l.$$", "")
      .replaceAll(" Software Company Limited$$", "")
      .replaceAll(" Limited$$", "")
      .replaceAll(" Software International$$", "")
      .replaceAll(" Europe SA$$", "")
      .replaceAll(" Europe B.V.$$", "")
      .replaceAll(" America Corporation", "")
      .replaceAll(" of America$$", "")
      .replaceAll(" Electronics Co.", "")
      .replaceAll(" Entertainment Co.$$", "")
      .replaceAll(" Industry Co.$$", "")
      .replaceAll(" Manufacturing Co.$$", "")
      .replaceAll(" Publishing Co.$$", "")
      .replaceAll(" Technology Co.$$", "")
      .replaceAll(" Trading Co.$$", "")
      .replaceAll(" Co. Development Group$$", "")
      .replaceAll(" Software Corp.$$", "")
      .replaceAll(" Entertainment Corp.$$", "")
      .replaceAll(" Entertainment Software$$", "")
      .replaceAll(" Leisure Corporation$$", "")
      .replaceAll(" Game Company$$", "")
      .replaceAll(" Co.$$", "")
      .replaceAll(" Corp.$$", "")
      .replaceAll(" Corporation$$", "")
      .replaceAll(" Company$$", "")
      .replaceAll(" Ltd$$", "")
      .replaceAll(" AB$$", "")
      .replaceAll(" CE$$", "")
      .replaceAll(" A/S$$", "")
      .replaceAll(" Multimedia SA$$", "")
      .replaceAll("^The ", "")
      .trim
    else part.trim
    if (res.isEmpty || Seq("Freeware", "Unknown").contains(res)) ""
    else res
  ).filter(_.nonEmpty).toBuffer
}

lazy val exodosMetas = metas.par.map(m =>
  val publishers_ = normalize(m.publisher)
  val developers = normalize(m.developer)
  val publishers = (publishers_ ++ developers).sorted.distinct
  var year = m.year
  val meta = MetaData(
    hash = "",
    authors = Buffer.empty,
    album = normalizeGame(m.game).trim,
    publishers = publishers,
    year = year,
    _type = "Game",
    _platform = "PC",
  )
  println(s"EXODOS META: ${meta}")
  meta
).toSet

val retroexo_by_path = sources.sourceDB(Source.RetroExo).groupBy(_.path.toLowerCase)

// XXX quirks or random unrelated files included (cractro musics etc.)
val fileBlacklist = Set(
  "CD-Man Version 2.0 (1989).zip",
  "Cell Block A (1999).zip",
  "Color Buster (1992).zip",
  "Crazy Sue (1990).zip",
  "Pinball Illusions (1995).zip",
  "Spaceward Ho! (1992).zip",
)
val exodosExtras = metas
.par
.filterNot(m => m.year > 0 && m.year <= 1991)
.filterNot(m => fileBlacklist.contains(m.file)
)
.map(meta =>
  val path = "exodos/" + meta.file.toLowerCase
  val md5s = sources.findArchive(path, retroexo_by_path).map(_._1).sorted.distinct
  md5s.distinct.map((_, (meta, md5s.distinct)))
).flatten.seq.groupBy(_._1).mapValues(_.map(_._2)).par.flatMap { case (md5, _metas) =>
  val minyear = _metas.map(m => if (m._1.year > 0) m._1.year else 9999).min
  val metas = _metas.filter(_._1.year <= minyear).map { case (meta, _) =>
    val publishers_ = normalize(meta.publisher)
    val developers = normalize(meta.developer)
    val publishers = (publishers_ ++ developers).sorted.distinct
    (meta.file, MetaData(
      hash = md5.take(12),
      authors = Buffer.empty,
      album = normalizeGame(meta.game).trim,
      publishers = publishers,
      year = meta.year,
      _type = "Game",
      _platform = "PC",
    ))
  }
  if (metas.isEmpty) {
    println(s"RETROEXO EXTRA: no meta for MD5 ${md5} metas: ${_metas}")
    None
  } else {
    Some(metas.sortBy(_._1).head)
  }
}.seq.toBuffer.distinct

for ((file, meta) <- exodosExtras) {
  println(s"EXODOS EXTRA META: $file - ${meta}")
}
