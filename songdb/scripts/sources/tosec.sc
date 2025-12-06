// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0
//> using dep org.scala-lang.modules::scala-xml::2.4.0

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.Using
import scala.util.boundary, boundary.break
import scala.xml.Elem
import scala.xml.factory.XMLLoader
import javax.xml.parsers.SAXParser

import convert._

object XML extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false);
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
    f.newSAXParser()
  }
}

case class TosecMeta(
  title: String,
  publishers: Buffer[String],
  year: Int,
)

val tosecDir = System.getProperty("user.home") + "/tosec/TOSEC/"
val tosecIsoDir = System.getProperty("user.home") + "/tosec/TOSEC-ISO/"

lazy val platforms = Buffer(
  //"3DO 3DO",
  //"Acorn Archimedes",
  //"Acorn Risc PC",
  //"Analogue Pocket",
  //"Apple Macintosh",
  "Atari Falcon030",
  "Atari Jaguar",
  //"Atari Lynx",
  //"Atari ST",
  //"Bandai WonderSwan",
  "Commodore Amiga",
  //"Fujitsu FM Towns",
  //"Game Park GP32",
  "IBM PC Compatibles",
  //"MEGA MEGA65",
  //"Microsoft Pocket PC",
  //"Microsoft Xbox",
  //"Nintendo 3DS",
  //"Nintendo 64",
  //"Nintendo DS",
  //"Nintendo Game Boy Advance",
  //"Nintendo GameCube",
  //"Nintendo Wii",
  //"Nokia N-Gage",
  //"Nokia N900",
  //"OpenPandora Pandora",
  //"Panic Playdate",
  //"Sega 32X",
  //"Sega Dreamcast",
  //"Sega Mega Drive",
  //"Sega Mega-CD",
  //"Sega Saturn",
  //"Sega WonderMega",
  //"Sharp X68000",
  //"Sony PlayStation",
)

lazy val dats = Seq(
  "Demos",
  "Diskmags",
  "Games",
  "Packmags",
)

lazy val tosecPattern = """^(.*) \((.*?)\)\((.*?)\)""".r
lazy val titleSuffixPattern = """\s*\((demo-playable)\)\s*$""".r
lazy val articlePattern = """^(.*),\s*(The|A|An)$""".r
lazy val namePattern = """^([^,]+),\s*(.+)$""".r

def normalizeTitle(title: String): String = {
  // Remove demo-related suffixes
  val cleaned = titleSuffixPattern.replaceFirstIn(title, "").trim
  
  // Move trailing article to front
  articlePattern.findFirstMatchIn(cleaned) match {
    case Some(m) => s"${m.group(2)} ${m.group(1)}"
    case None => cleaned
  }
}


def normalizePublisher(publisher: String): String = {
  // Check if it's in "LastName, FirstName" format
  namePattern.findFirstMatchIn(publisher) match {
    case Some(m) => s"${m.group(2)} ${m.group(1)}"
    case None => publisher
  }
}

lazy val allMetas = Seq(tosecDir, tosecIsoDir).par.flatMap(dir =>
  Files.list(Paths.get(dir)).toScala(Buffer).sorted
  .filter(e => platforms.exists(p => e.getFileName.toString.startsWith(p)))
  .filter(e => dats.exists(d => e.getFileName.toString.contains(s" $d ")))
  .filterNot(_.getFileName.toString.contains(" - Compilations - "))
  .flatMap(f =>
    //println(s"Processing $dir file: $f")
    val dat = XML.loadFile(f.toFile)
    (dat \ "game").flatMap(g =>
      val name = (g \ "@name").text.trim
      val (title, year, publishers) = tosecPattern.findFirstMatchIn(name) match {
        case Some(m) =>
          val rawTitle = m.group(1).trim
          val title = normalizeTitle(rawTitle)
          val date = m.group(2).trim
          val publishers = m.group(3).trim.split(" - ").map(_.trim)
            .filterNot(_ == "-")
            .map(normalizePublisher)
            .sorted.distinct
            .toBuffer
          val year = date.take(4).toIntOption.getOrElse(0)
          if (title.startsWith("ZZZ-UNK")) ("", 0, Buffer.empty)
          else (title, year, publishers)
        case None =>
          //println(s"TOSEC META: ${name} => FAILED TO PARSE")
          ("", 0, Buffer.empty)
      }
      if (title.nonEmpty || year != 0 || publishers.nonEmpty) {
        val meta = TosecMeta(
          title,
          publishers,
          year,
        )
        //println(s"TOSEC META: ${name} => ${meta}")
        Some(meta)
      } else None
    )
  ).toSet).toSet.seq

lazy val metas = allMetas.par.flatMap(m =>

  def deduplicatePublishers(publishers: Buffer[String]): Buffer[String] = {
    // Group publishers that are the same with/without "The" prefix
    val grouped = publishers.groupBy(p => 
      if (p.startsWith("The ")) p.drop(4) else p
    )
  
    // For each group, prefer the version with "The" if it exists
    grouped.values.map { group =>
      group.find(_.startsWith("The ")).getOrElse(group.head)
    }.toBuffer.sorted
  }

  if (allMetas.exists(n => n != m && n.title == m.title && n.year == m.year &&
      n.publishers != m.publishers)) {
    //println(s"TOSEC META PUBLISHERS CONFLICT: ${m} vs ${allMetas.filter(n => n != m && n.title == m.title && n.year == m.year)}")
    // merge publishers
    val mergedPublishers = deduplicatePublishers(m.publishers ++ allMetas.filter(n => n != m && n.title == m.title && n.year == m.year)
      .flatMap(_.publishers)).distinct.sorted
    Some(m.copy(publishers = mergedPublishers))

  } else if (allMetas.exists(n => n != m && n.title == m.title && n.publishers == m.publishers &&
      n.year != m.year)) {
    //println(s"TOSEC META YEAR CONFLICT: ${m} vs ${allMetas.filter(n => n != m && n.title == m.title && n.publishers == m.publishers)}")
    // keep oldest year
    val oldestYear = allMetas.filter(n => n != m && n.title == m.title && n.publishers == m.publishers)
      .map(_.year).filter(_ > 0).minOption
    if (oldestYear.isDefined && (m.year == 0 || oldestYear.get < m.year)) {
      Some(m.copy(year = oldestYear.get))
    } else Some(m)

  } else Some(m)
)

for (m <- metas) {
  //println(s"FINAL TOSEC META: ${m}")
}

lazy val tosecMetas = metas.map(m =>
  MetaData(
    hash = "",
    authors = Buffer.empty,
    publishers = m.publishers,
    album = m.title,
    year = m.year,
  )
)
