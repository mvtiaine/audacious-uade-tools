// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>

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

case class WHDLoadMeta(
  fullName: String,
  shortName: String,
  `type`: String,
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
  // ...
)

val whdloaddb_csv = System.getProperty("user.home") + "/whdload/WHDLoad_Database.csv"

lazy val metas = Using(scala.io.Source.fromFile(whdloaddb_csv)(using scala.io.Codec.ISO8859))(_.getLines.toBuffer.par.map { line =>
  val l = line.split(";")
  WHDLoadMeta(
    fullName = l(0).trim,
    shortName = l(1).trim,
    `type` = l(2).trim,
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
  )
}).get

lazy val articlePattern = """^(.*),\s*(The|A|An)$""".r

def normalize(s: String): String = {
  // Remove any text inside parentheses
  val withoutParens = s.replaceAll("""\([^)]*\)""", "").trim
  // Move trailing article to front
  articlePattern.findFirstMatchIn(withoutParens) match {
    case Some(m) => s"${m.group(2)} ${m.group(1)}"
    case None => withoutParens
  }
}

lazy val whdloadMetas = metas.par.flatMap(m =>
  val publishers = Buffer(m.producer, m.developer).map(normalize).flatMap(p =>
    p.split("  ")
  ).map(_.trim).filter(_.nonEmpty).distinct.sorted
  Seq(normalize(m.shortName), normalize(m.fullName)).distinct.map(name =>
    val meta = MetaData(
      hash = "",
      authors = Buffer.empty,
      album = name,
      publishers = publishers,
      year = m.year,
    )
    //println(s"WHDLOAD META: ${meta}")
    meta
  )
).toSet
