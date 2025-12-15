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
  _type: String = "",
  platform: String = "",
)

val tosecDir = System.getProperty("user.home") + "/tosec/TOSEC/"
val tosecIsoDir = System.getProperty("user.home") + "/tosec/TOSEC-ISO/"
val tosecCUEsDir = System.getProperty("user.home") + "/tosec/CUEs/"

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

lazy val cuesDirs = Seq(
  //"3DO/3DO Interactive Multiplayer/Games/",
  //"3DO/3DO Interactive Multiplayer/Homebrew/Demos/",
  //"3DO/3DO Interactive Multiplayer/Homebrew/Games/",
  //"American Laser Games/CD-ROM System/Games/",
  //"Apple/Macintosh/CD/Games/",
  "Atari/Jaguar CD/Homebrew/Games/",
  //"Bandai/Playdia/Games/",
  "Commodore/Amiga CD32/Games/",
  "Commodore/Amiga CD32/Homebrew/Games/",
  "Commodore/Amiga CDTV/Games/",
  "Commodore/Amiga CDTV/Homebrew/Games/",
  "Commodore/Amiga/CD/Games/",
  "IBM/PC Compatibles/CD/Games/",
  //"Fujitsu/FM Towns/CD/Demos/",
  //"Fujitsu/FM Towns/CD/Games/",
  //"NEC/PC-Engine CD & TurboGrafx-16 CD/Games/"
  //"NEC/PC-Engine CD & TurboGrafx-16 CD/Homebrew/Games/"
  //"NEC/PC-FX/Demos/",
  //"Philips/CD-i/Homebrew/Demos/",
  //"Philips/CD-i/Homebrew/Games/"
  //"Sega/32X/CD/Games/"
  //"Sega/Dreamcast/Homebrew/Games/",
  //"Sega/Mega-CD & Sega CD/CD/Games/"
  //"Sega/Mega-CD & Sega CD/Homebrew/Demos/"
  //"Sega/Mega-CD & Sega CD/Homebrew/Games/",
  //"Sega/Saturn/Games/",
  //"Sega/Saturn/Homebrew/Games/",
  //"SNK/Neo-Geo CD/Demos/",
  //"SNK/Neo-Geo CD/Games/",
  //"Sony/PlayStation 2/Homebrew/Games/",
  //"Sony/PlayStation Portable/Homebrew/Games/",
  //"Sony/PlayStation/Demos/"
  //"Sony/PlayStation/Games/",
  //"Sony/PlayStation/Homebrew/Games/"
)

def normalizePlatform(platform: String): String = {
  if (platform.startsWith("Atari")) "Atari"
  else if (platform.startsWith("Commodore")) "Amiga"
  else if (platform.startsWith("IBM")) "PC"
  else ""
}

lazy val tosecPattern = """^(.*) \((.*?)\)\((.*?)\)""".r
lazy val titleSuffixPattern = """\s*\((demo-playable)\)\s*$""".r
lazy val articlePattern = """^(.*), (The|A|An)\b(.*)""".r
lazy val namePattern = """^([^,]+),\s*(.+)$""".r

def normalizeTitle(title: String): String = {
  // Remove demo-related suffixes
  val cleaned = titleSuffixPattern.replaceFirstIn(title, "").trim
  
  articlePattern.findFirstMatchIn(cleaned) match {
    case Some(m) => s"${m.group(2)} ${m.group(1)}${m.group(3)}".trim
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

lazy val datMetas = Seq(tosecDir, tosecIsoDir).par.flatMap(dir =>
  Files.list(Paths.get(dir)).toScala(Buffer).sorted
  .filter(e => platforms.exists(p => e.getFileName.toString.startsWith(p)))
  .filter(e => dats.exists(d => e.getFileName.toString.contains(s" $d ")))
  .filterNot(_.getFileName.toString.contains(" - Compilations - "))
  .flatMap(f =>
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
          ("", 0, Buffer.empty)
      }
      if (title.nonEmpty || year != 0 || publishers.nonEmpty) {
        val _type = if (f.getFileName.toString.contains(" - Games ")) "Game"
          else if (f.getFileName.toString.contains(" - Demos ")) "Demo"
          else ""
        val meta = TosecMeta(
          title,
          publishers,
          year,
          _type,
          normalizePlatform(f.getFileName.toString)
        )
        Some(meta)
      } else None
    )
  ).toSet).toSet.seq

lazy val cuesMetas = cuesDirs.par.flatMap(dir =>
  val fullDir = Paths.get(tosecCUEsDir, dir)
  val files = {
    var cues = Buffer.empty[String]
    if (Files.exists(fullDir)) {
      val list = Files.list(fullDir)
      for (f <- list.toScala(Buffer)) {
        if (f.toFile.isDirectory) {
          cues ++= Files.list(f).toScala(Buffer).map(_.getFileName.toString)
        } else {
          cues += f.getFileName.toString
        }
      }
    }
    cues.sorted.distinct
  }
  files.flatMap(filename =>
    val name = filename.trim
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
        ("", 0, Buffer.empty)
    }
    if (title.nonEmpty || year != 0 || publishers.nonEmpty) {
      val _type = if (dir.contains("/Games/")) "Game"
        else if (dir.contains("/Demos/")) "Demo"
        else ""
      val meta = TosecMeta(
        title,
        publishers,
        year,
        _type,
        normalizePlatform(dir)
      )
      Some(meta)
    } else None
  ).toSet
).toSet.seq

lazy val metas = datMetas ++ cuesMetas
lazy val tosecMetas = metas.map(m =>
  MetaData(
    hash = "",
    authors = Buffer.empty,
    publishers = m.publishers.sorted.distinct.toBuffer,
    album = m.title.trim,
    year = m.year,
    _type = m._type.trim,
    _platform = m.platform.trim,
  )
)
