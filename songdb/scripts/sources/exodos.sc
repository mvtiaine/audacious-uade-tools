// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2026 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

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

// eXoID   MobyID  Game    File    Folder  Year    Publisher       Developer       Genre   Perspective     Misc    Series  URL     Collection      Notes   Compilation?    Foreign?        Add-On Included?
case class eXoDOSMeta(
    //exoID: Int,
    //mobyID: Int,
    game: String,
    //file: String,
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

val dosmaster_tsv = System.getProperty("user.home") + "/exodos/DOS_Master - DOS_Master.tsv"

lazy val metas = Using(scala.io.Source.fromFile(dosmaster_tsv)(using scala.io.Codec.UTF8))(_.getLines.toBuffer.par.map { line =>
  val l = line.split("\\t")
  eXoDOSMeta(
    game = l(2).trim,
    year = l(5).toIntOption.getOrElse(0),
    publisher = l(6).trim,
    developer = l(7).trim,
  )
}).get

lazy val articlePattern1 = """^(.*), (The|A|An|El), (.*)""".r
lazy val articlePattern2 = """^(.*), (The|A|An|El)$""".r
lazy val articlePattern3 = """^(.*), (The|A|An|El) - (.*)""".r
lazy val articlePattern4 = """^(.*), (The|A|An|El) / (.*)""".r
lazy val articlePattern5 = """^(.*), (The|A|An|El): (.*)""".r

def normalizeGame(s: String): String = {
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

def normalize(s: String): Buffer[String] = {
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
  val meta = MetaData(
    hash = "",
    authors = Buffer.empty,
    album = normalizeGame(m.game).trim,
    publishers = publishers,
    year = m.year,
    _type = "Game",
    _platform = "PC",
  )
  meta
).toSet
