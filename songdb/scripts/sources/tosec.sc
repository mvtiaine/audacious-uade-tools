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
import normalization._

object XML extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false);
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
    f.newSAXParser()
  }
}

final case class TosecMeta(
  title: String,
  publishers: Buffer[String],
  year: Int,
  _type: String = "",
  _platform: String = "",
  _demo: Boolean = false,
  _compilation: Boolean = false,
  _crack: Boolean = false,
  _budget: Boolean = false,
  _nonEnglish: Boolean = false,
)

val tosecDir = System.getProperty("user.home") + "/sources/metadata/tosec/TOSEC/"
val tosecIsoDir = System.getProperty("user.home") + "/sources/metadata/tosec/TOSEC-ISO/"
val tosecCUEsDir = System.getProperty("user.home") + "/sources/metadata/tosec/CUEs/"

lazy val platforms = Buffer(
  //"3DO 3DO",
  //"Acorn Archimedes",
  //"Acorn Risc PC",
  //"Analogue Pocket",
  //"Apple Macintosh",
  "Atari Falcon030",
  "Atari Jaguar",
  //"Atari Lynx",
  "Atari ST",
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

private def normalizePlatform(platform: String): String = {
  if (platform.startsWith("Atari")) "Atari"
  else if (platform.startsWith("Commodore")) "Amiga"
  else if (platform.startsWith("IBM")) "PC"
  else ""
}

lazy val tosecPattern = """^(.*) \((.*?)\)\((.*?)\)""".r
lazy val titleSuffixPattern = """\s*\((demo|demo-kiosk|demo-playable|demo-rolling|demo-slideshow)\)\s*$""".r
lazy val articlePattern = """^(.*), (The|A|An)\b(.*)""".r
lazy val namePattern = """^([^,]+),\s*(.+)$""".r

private def normalizeTitle(title: String): String = {
  // Remove demo-related suffixes
  val cleaned = titleSuffixPattern.replaceFirstIn(title, "").trim
  
  articlePattern.findFirstMatchIn(cleaned) match {
    case Some(m) => s"${m.group(2)} ${m.group(1)}${m.group(3)}".trim
    case None => cleaned
  }
}

private def normalizePublisher(publisher: String): String = {
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
      val (title, year, publishers, demo, compilation, crack, budget, nonEnglish) = tosecPattern.findFirstMatchIn(name) match {
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
          val demo = Set("(demo)", "(demo-kiosk)", "(demo-playable)", "(demo-rolling)", "(demo-slideshow)").exists(rawTitle.contains)
          val compilation = name.contains("[compilation ") || name.contains("[compilation]")
          val crackVersion = Set("[a ","[a]","[cr ","[cr]","[f ","[f]","[h ","[h]","[m ","[m]","[p ","[p]","[t ","[t]","[tr ","[tr]").exists(name.contains)
          val budget = name.contains("[budget ") || name.contains("[budget]")
          val nonEnglish = Set("(de)", "(fr)", "(it)", "(es)", "(pl)").exists(name.toLowerCase.contains)

          if (title.startsWith("ZZZ-UNK")) ("", 0, Buffer.empty, false, false, false, false, false)
          else (title, year, publishers, demo, compilation, crackVersion, budget, nonEnglish)
        case None =>
          ("", 0, Buffer.empty, false, false, false, false, false)
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
          normalizePlatform(f.getFileName.toString),
          demo,
          compilation,
          crack,
          budget,
          nonEnglish
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
    val (title, year, publishers, demo, compilation, crack, budget, nonEnglish) = tosecPattern.findFirstMatchIn(name) match {
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
        val demo = Set("demo", "demo-kiosk", "demo-playable", "demo-rolling", "demo-slideshow").exists(rawTitle.contains)
        val compilation = name.contains("[compilation ") || name.contains("[compilation]")
        val crackVersion = Set("[a ","[a]","[cr ","[cr]","[f ","[f]","[h ","[h]","[m ","[m]","[p ","[p]","[t ","[t]","[tr ","[tr]").exists(name.contains)
        val budget = name.contains("[budget ") || name.contains("[budget]")
        val nonEnglish = Set("(de)", "(fr)", "(it)", "(es)", "(pl)").exists(name.toLowerCase.contains)

        if (title.startsWith("ZZZ-UNK")) ("", 0, Buffer.empty, false, false, false, false, false)
        else (title, year, publishers, demo, compilation, crackVersion, budget, nonEnglish)
      case None =>
        ("", 0, Buffer.empty, false, false, false, false, false)
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
        normalizePlatform(dir),
        demo,
        compilation,
        crack,
        budget,
        nonEnglish
      )
      Some(meta)
    } else None
  ).toSet
).toSet.seq

private val metas = (datMetas ++ cuesMetas)
  .filterNot(m => (m._type == "Game" && m._platform == "PC" && (m.year > 0 && m.year <= 1991)))
  .filterNot(m => (m._platform == "PC" && (m.year > 0 && m.year < 1990)))
  .filterNot(m => (m._platform == "Atari" && (m.year > 0 && m.year < 1988)))

lazy val originals = metas.toSeq.sortBy(m => -m.year).filter(m => !m._crack && !m._demo && !m._compilation && !m._budget && !m._nonEnglish)
  .groupBy(m => (m._platform, m._type, _normalizeAlbum(m.title), m.publishers.sorted.distinct))

lazy val others = metas.toSeq.sortBy(m => -m.year).filter(m => m._crack || m._demo || m._compilation || m._budget || m._nonEnglish)
  .groupBy(m => (m._platform, m._type, _normalizeAlbum(m.title), m.publishers.sorted.distinct))

lazy val tosecMetas = (
  originals.par.flatMap { case ((platform, _type, normAlbum, publishers), metas) =>
  if (metas.size > 1) {
    println(s"TOSEC META (original): ${metas.head} has ${metas.size} entries with same platform/type/normalized album/publishers, filtering out duplicates: ${metas.mkString(", ")}")
  }
  Some(metas.sortBy(m => if (m.year > 0) m.year else Int.MaxValue).head)
} ++ others.par.flatMap { case ((platform, _type, normAlbum, publishers), metas) =>
  val _originals = originals.get((platform, _type, normAlbum, publishers)).getOrElse(Seq.empty)
  if (_originals.nonEmpty && (_originals.exists(_.year > 0) || !metas.exists(_.year > 0))) {
    println(s"TOSEC META (other): ${metas.head} has same platform/type/normalized album/publishers as original metadata, filtering out: ${metas.mkString(", ")} originals: ${_originals.mkString(", ")}")
    None

  } else {
    val meta = metas.sortBy(m => if (m.year > 0) m.year else Int.MaxValue).head
    if (metas.size > 1) {
      println(s"TOSEC META (other): ${meta} has ${metas.size} entries with same platform/type/normalized album/publishers, filtering out duplicates: ${metas.mkString(", ")}")
      Some(meta)
    } else Some(meta)
  }
}).map(m =>
  val _platform = m._platform.trim
  val _type = m._type.trim
  var title = m.title.trim
  var publishers = m.publishers.sorted.distinct.toBuffer
  var year = m.year
  // XXX quirks
  if (_platform == "Atari" && _type == "Game" && title == "Tusker") year = 1989
  else if (_platform == "Amiga" && _type == "Game" && title == "Joe & Mac - Caveman Ninja") year = 1993
  else if (_platform == "Amiga" && _type == "Game" && title == "Charlie J Cool") year = 1996
  else if (_platform == "Atari" && _type == "Game" && title == "International Ninja Rabbits") year = 1991
  else if (_platform == "Atari" && _type == "Game" && title == "Zero 5") year = 1994
  else if (_platform == "Atari" && _type == "Game" && title == "5th Gear") year = 1989
  else if (_platform == "Atari" && _type == "Game" && title == "Operation Thunderbolt") year = 1989
  else if (_platform == "Atari" && _type == "Game" && title == "Bio Challenge") year = 1989
  else if (_platform == "Amiga" && _type == "Game" && title.toLowerCase == "james pond 2 - codename robocod") year = 1991
  else if (_platform == "Amiga" && _type == "Game" && title.toLowerCase == "james pond 2 - robocod") year = 1991
  else if (_platform == "Amiga" && _type == "Game" && title == "Joe Blade 2") year = 1988
  else if (_platform == "Amiga" && _type == "Demo" && title == "Zenith Slide-Show") year = 1991
  else if (_platform == "Amiga" && _type == "Game" && title == "Pinball Fantasies") year = 1992
  else if (_platform == "Amiga" && _type == "Game" && title == "TV Sports Basketball") year = 1990
  else if (_platform == "Amiga" && _type == "Game" && title.startsWith("Mine Runner")) year = 1993
  else if (_platform == "Amiga" && _type == "Game" && title.startsWith("Uropa 2 - The Ulterior Colony")) year = 1997
  else if (_platform == "Amiga" && _type == "Game" && title == "Prospector - In the Mazes of Xor") year = 1989
  else if (_platform == "Amiga" && _type == "Game" && title == "Hole-In-One") year = 1989
  else if (_platform == "Amiga" && _type == "Game" && title == "Star Trash" && year == 1989) year = 1990
  else if (_platform == "Amiga" && _type == "Game" && title == "Desert Wolf") year = 1996
  else if (_platform == "Amiga" && _type == "Game" && title == "The Basket Manager") year = 1990
  else if (_platform == "Amiga" && _type == "Game" && title == "Deluxe Galaga") year = 1993
  else if (_platform == "Amiga" && _type == "Game" && title == "The Big Red Adventure") year = 1997
  else if (_platform == "Amiga" && _type == "Game" && title == "Elvira II - Jaws of Cerberus rev 1") {
    title = "Elvira II - The Jaws of Cerberus"
    year = 1992
  } else if (_platform == "Amiga" && _type == "Game" && title == "The Games - Winter Edition") {
    publishers = Buffer("Epyx", "FACS Entertainment")
    year = 1989
  } else if (_platform == "Amiga" && _type == "Game" && (title == "Step Five") || title == "StepFive") {
    publishers = Buffer("NightLight")
    year = 1994
  } else if (_platform == "Amiga" && _type == "Game" && title == "Wolfen" && year == 1992) {
    publishers = Buffer("Apocalypse", "OASE")
  } else if (_platform == "Atari" && _type == "Game" && title == "Stormlords" && year == 1989) title = "Stormlord"
  MetaData(
    hash = "",
    authors = Buffer.empty,
    publishers = publishers,
    album = title,
    year = year,
    _type = _type,
    _platform = _platform,
  )
)
// XXX
.filterNot(m =>
  (m._platform == "Amiga" && m._type == "Game" && m.album == "Amnios" && m.publishers == Buffer("Microdeal") && m.year == 1989) ||
  (m._platform == "Amiga" && m._type == "Game" && m.album == "MineRunner v1.8" && m.publishers == Buffer("Matthias Bock") && m.year == 1995) ||
  (m._platform == "Amiga" && m._type == "Game" && m.album == "Another World" && m.publishers == Buffer("Kixx") && m.year == 1995) ||
  (m._platform == "Amiga" && m._type == "Game" && m.album == "Fire Force" && m.publishers == Buffer("Buzz") && m.year == 1993) ||
  (m._platform == "Atari" && m._type == "Game" && m.album == "Elf" && m.publishers == Buffer("MicroValue") && m.year == 1988) ||
  (m._platform == "Atari" && m._type == "Game" && m.album == "Spaceball" && m.publishers == Buffer("Microvideo") && m.year == 1989) ||
  (m._platform == "Atari" && m._type == "Game" && m.album == "International Soccer Challenge" && m.publishers == Buffer("Micro Style") && m.year == 1988) ||
  (m._platform == "Amiga" && m._type == "Game" && m.album == "Startrash" && m.publishers == Buffer("Top Shots") && m.year == 1992)
).toSet.seq
