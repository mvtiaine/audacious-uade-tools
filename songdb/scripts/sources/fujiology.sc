// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0
//> using dep org.apache.poi:poi-ooxml:5.5.0

import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.util.Using
import java.io.File
import java.io.FileInputStream
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.ss.usermodel.Row
import org.apache.poi.xssf.usermodel.XSSFSheet
import org.apache.poi.xssf.usermodel.XSSFWorkbook

//val fujiology_xlsx = System.getProperty("user.home") + "/fujiology/fujiology_archive_2_9_7.xlsx"
// XXX corrupted fujiology excel file?
org.apache.poi.util.IOUtils.setByteArrayMaxOverride(1_000_000_000);
val fujiology_xlsx = System.getProperty("user.home") + "/sources/fujiology/fujiology_archive_2_9_9.xlsx"
lazy val fujiology_by_filename = sources.fujiology.groupBy(_.path.split("/").last.toLowerCase)

case class FujiologyMeta (
  md5: String,
  authors: Buffer[String],
  publishers: Buffer[String],
  album: String,
  year: Option[Int],
  system: String,
  prodType: String,
  // TODO composer?
)

def normalizePlatform(platform: String): String = {
  if (platform.startsWith("Amiga")) "Amiga"
  else if (platform.startsWith("Atari")) "Atari"
  else if (platform.startsWith("FALCON")) "Atari"
  else if (platform.startsWith("JAGUAR")) "Atari"
  else if (platform.startsWith("TT")) "Atari"
  else if (platform.startsWith("Windows")) "PC"
  else if (platform.startsWith("MS-DOS")) "PC"
  else ""
}

def getCellString(row: Row, cellIndex: Int): String = {
    val cell = row.getCell(cellIndex)
    if (cell == null) {
      ""
    } else {
      val value = cell.getCellType() match {
        case CellType.STRING => cell.getStringCellValue().trim
        case CellType.NUMERIC => cell.getNumericCellValue().toInt.toString
        case _ => ""
      }
      if (value == "-") "" else value
    }
}

def splitGroups(s: String) = {
  s
  .replaceAll("\\s*\\[.*?\\]\\s*", " ")
  .split("/| & | \\+ ")
  .map { name =>
    if (name.contains(", ")) {
      val parts = name.split(", ", 2)
      s"${parts(1)} ${parts(0)}"
    } else {
      name
    }
  }
  .map(_.trim)
  .filterNot(_ == "Etc.")
  .filterNot(_ == "Inc.")
}

def splitComposers(s: String) = {
  s
  .replaceAll("\\s*\\[.*?\\]\\s*", " ")
  .replaceAll("\\s*\\(\\+\\)\\s*", " ")
  .replaceAll("\\s*/\\s*.*", "")
  .split(" & | \\+ ").map(_.trim)
  .filterNot(_.startsWith("Unknown"))
  .filterNot(_.isEmpty)
  .map { name =>
    if (name.contains(", ")) {
      val parts = name.split(", ", 2)
      s"${parts(1)} ${parts(0)}"
    } else {
      name
    }
  }
  .map(_.trim)
}

lazy val music_metas = {
  val metas = Buffer[FujiologyMeta]()
  val by_filename = mutable.Map.from(fujiology_by_filename)
  val file = new FileInputStream(new File(fujiology_xlsx))
  val workbook = new XSSFWorkbook(file)
  //val sheet = workbook.getSheet("MUSIC")
  val sheet = workbook.getSheet("MUSIC FT-SNDH-ASMA")
  var prevfolder = ""
  val rows = sheet.iterator()
  rows.next()
  while (rows.hasNext()) {
    val row = rows.next()
    if (row.getLastCellNum() >= 4) {
      val composer = getCellString(row, 0)
      val songname = getCellString(row, 1)
      val filenames = getCellString(row, 3)
        .toLowerCase
        .split(",")
        .map(_.replace("#","_"))
        .map(_.trim)
        .filterNot(_.isEmpty)
      val format = getCellString(row, 4)
      val system = getCellString(row, 5)
      val prod = getCellString(row, 6)
      val crew = getCellString(row, 7)
      var foldername = getCellString(row, 10)
      if (!foldername.isEmpty) {
        prevfolder = foldername
      }
      val composers = splitComposers(composer)
      val publishers = splitGroups(crew)

      filenames.foreach { filename_ =>
        var filename = filename_
        var entries = by_filename.getOrElse(filename, Seq())
          .filter(_.path.startsWith("MUSIC/"))
        
        if (entries.isEmpty && filename.endsWith(".zip")) {
          var altname = ""
          format match {
            case mod if format.startsWith("mod") =>
              altname = filename.replace(".zip", ".mod")
            case "EPSS" =>
              altname = filename.replace(".zip", ".spi")
            case "quartet [4q]" =>
              altname = filename.replace(".zip", ".4q")
            case "quartet [4Q]" =>
              altname = filename.replace(".zip", ".4q")
            case "stonetracker" =>
              altname = filename.replace(".zip", ".spm")
            case other =>
              altname = filename.replace(".zip", s".${other.toLowerCase}")
          }
          if (!altname.isEmpty) {
            entries = by_filename.getOrElse(altname, Seq())
              .filter(_.path.startsWith("MUSIC/"))
            if (!entries.isEmpty) {
              filename = altname
            }
          }
        }

        if (entries.isEmpty && !Seq("SNDH","SAP","sid","mp3","mp2","ogg","flac","wav","mid","avi").contains(format)) {
          System.err.println(s"WARN: Fujiology no MUSIC matches for '${filename}' composer '${composer}' folder '${foldername}'")
        }
        if (entries.size > 1) {
          val origEntries = entries
          def normalize(s: String) = s
            .replaceAll("\\s*\\([^)]*\\)\\s*", "")
            .replaceAll("[^A-Za-z0-9/]","")
            .toLowerCase
          if (!entries.isEmpty && entries.forall(_.md5 == entries.head.md5)) {
            entries = Seq(entries.head)
          }
          if (entries.size > 1 && !foldername.isEmpty) {
            val filteredEntries = entries.filter(e => normalize(e.path).contains(s"/${normalize(foldername)}/"))
            if (!filteredEntries.isEmpty) {
              entries = filteredEntries
            }
          }
          if (!entries.isEmpty && entries.forall(_.md5 == entries.head.md5)) {
            entries = Seq(entries.head)
          }
          if (entries.size > 1 && !composers.isEmpty) {
            val filteredEntries = entries.filter(e => normalize(e.path).contains(s"/${normalize(composers.head).take(8)}/"))
            if (!filteredEntries.isEmpty) {
              entries = filteredEntries
            }
          }
          if (entries.size > 1 && !prevfolder.isEmpty) {
            val filteredEntries = entries.filter(e => normalize(e.path).contains(s"/${normalize(prevfolder)}/"))
            if (!filteredEntries.isEmpty) {
              entries = filteredEntries
            }
          }
          if (!entries.isEmpty && entries.forall(_.md5 == entries.head.md5)) {
            entries = Seq(entries.head)
          }
          if (entries.isEmpty) {
            System.err.println(s"WARN: Fujiology no MUSIC matches after filtering for '${filename}' composer '${composer}' folder '${foldername}': ${origEntries}")
          } else if (entries.size > 1) {
            System.err.println(s"WARN: Fujiology multiple MUSIC matches for '${filename}' composer '${composer}' folder '${foldername}': ${origEntries}")
          }
        }
        if (entries.size == 1) {
          val entry = entries.head
          val meta = FujiologyMeta(
            md5 = entry.md5,
            authors = composers.filterNot(_.isEmpty).sorted.distinct.toBuffer,
            publishers = publishers.filterNot(_.isEmpty).sorted.distinct.toBuffer,
            album = prod,
            year = None,
            system,
            prodType = "",
          )
          if (metas.exists(m => m.md5 == entry.md5)) {
            System.err.println(s"WARN: Fujiology duplicates: ${meta} vs ${metas.filter(_.md5 == entry.md5)}")
          }
          metas += meta
          by_filename.update(filename, by_filename(filename).filterNot(_ == entry))
        }
      }
    }
  }
  file.close()
  metas
}

lazy val prods_metas = {
  val metas = Buffer[FujiologyMeta]()
  val by_filename = mutable.Map.from(fujiology_by_filename)
  val file = new FileInputStream(new File(fujiology_xlsx))
  val workbook = new XSSFWorkbook(file)
  case class ProdRow (
    system: String,
    prod: String,
    filename: String,
    publishers: Buffer[String],
    prodType: String,
  )

  def parseRow(row: Row, system: String, prodRows: Buffer[ProdRow], filenameIdx: Int) = {
    if (row.getLastCellNum() >= filenameIdx) {
      val group = getCellString(row, 0)
      val prod = getCellString(row, 1)
      val prodType = getCellString(row, 2)
      val filenames = getCellString(row, filenameIdx).toLowerCase.split(",|\\. ").map(_.trim)
        .filterNot(_.isEmpty)
      val publishers = splitGroups(group)
      filenames.foreach { filename =>
        prodRows += ProdRow(
          system,
          prod,
          filename,
          publishers.toBuffer,
          prodType
        )
      }
    }
  }
  def parseSheet(sheet: XSSFSheet, system: String, filenameIdx: Int = 6): Buffer[ProdRow] = {
    val prodRows = Buffer[ProdRow]()
    val rows = sheet.iterator()
    rows.next()
    while (rows.hasNext()) {
      parseRow(rows.next(), system, prodRows, filenameIdx)
    }
    prodRows
  }

  def parseMetas(prefix: String, prodRows: Buffer[ProdRow], index: Int) = {
    val metas = Buffer[FujiologyMeta]()
    val prodsByFilename = prodRows.groupBy(_.filename)
    sources.fujiology.filter(e => e.path.startsWith(prefix)).foreach{ e =>
      var i = index
      if (e.path.startsWith(s"${prefix}!BONUS/")) {
        i += 1
      }
      var filename = e.path.split("/")(i).toLowerCase
      val prods = prodsByFilename.getOrElse(filename, Seq())
      if (prods.size == 1) {
        val prod = prods.head
        metas += FujiologyMeta(
          md5 = e.md5,
          authors = Buffer.empty,
          publishers = prod.publishers.sorted.distinct.toBuffer,
          album = prod.prod,
          year = None,
          system = prod.system,
          prodType = prod.prodType,
        )
      } else if (prods.size > 1) {
        System.err.println(s"WARN: Fujiology ${prefix} PRODS multiple entries for filename '${filename}': ${prods}")
      } else {
        System.err.println(s"WARN: Fujiology ${prefix} PRODS no entry for filename '${filename}': ${e}")
      }
    }
    metas
  }

  var system = "Atari ST/E"
  val stRows = parseSheet(workbook.getSheetAt(0), system, 5)
  metas ++= parseMetas("ST/", stRows, 3)

  system = "FALCON 030-060"
  val falconRows = parseSheet(workbook.getSheet(system), system)
  metas ++= parseMetas("FALCON/", falconRows, 2)
  
  system = "JAGUAR"
  val jaguarRows = parseSheet(workbook.getSheet(system), system)
  metas ++= parseMetas("JAGUAR/", jaguarRows, 2)
/*
  system = "LYNX"
  val lynxRows = parseSheet(workbook.getSheet(system), system)
  metas ++= parseMetas("LYNX/", lynxRows, 2)
*/
  system = "TT"
  val ttRows = parseSheet(workbook.getSheet(system), system)
  metas ++= parseMetas("TT/", ttRows, 2)

  file.close()
  metas
}

lazy val mags_metas = {
  val metas = Buffer[FujiologyMeta]()
  val file = new FileInputStream(new File(fujiology_xlsx))
  val workbook = new XSSFWorkbook(file)
  val sheet = workbook.getSheet("MAGAZINES")
  val rows = sheet.iterator()
  rows.next()
  case class MagRow (
    prod: String,
    system: String,
    filename: String,
    publishers: Buffer[String],
    prodType: String,
  )
  val magRows = Buffer[MagRow]()
  while (rows.hasNext()) {
    val row = rows.next()
    if (row.getLastCellNum() >= 8) {
      val prod = getCellString(row, 0)
        .replace(" - All Issues", "")
      val group = getCellString(row, 1)
      val prodType = getCellString(row, 2)
      val system = getCellString(row, 3)
      val filenames = getCellString(row, 7).toLowerCase.split(",|\\. ").map(_.trim)
        .filterNot(_.isEmpty)
      val publishers = splitGroups(group)
      filenames.foreach { filename =>
        magRows += MagRow(
          prod,
          system,
          filename,
          publishers.toBuffer,
          prodType
        )
      }
    }
  }
  val magsByFilename = magRows.groupBy(_.filename)
  sources.fujiology.filter(_.path.startsWith("MAGS/")).foreach{ e =>
    var filename = e.path.split("/")(2).toLowerCase
    if (filename == "falcon") {
      filename = e.path.split("/")(3).toLowerCase
    }
    val mags = magsByFilename.getOrElse(filename, Seq())
    if (mags.nonEmpty) {
      if (mags.size > 1) {
        System.err.println(s"WARN: Fujiology MAGS multiple entries for filename '${filename}': ${mags}")
      }
      val mag = mags.head
      metas += FujiologyMeta(
        md5 = e.md5,
        authors = Buffer.empty,
        publishers = mag.publishers.sorted.distinct.toBuffer,
        album = mag.prod,
        year = None,
        system = mag.system,
        prodType = mag.prodType,
      )
    } else {
      System.err.println(s"WARN: Fujiology MAGS no entry for filename '${filename}': ${e}")
    }
  }
  file.close()
  metas
}

lazy val party_metas = sources.fujiology.filter(_.path.startsWith("PARTIES/")).flatMap { e =>
  val dirs = e.path.split("/")
  val competitions = Seq("CHIPTUNE", "F030MSX", "M1CH", "M4CH", "M8CH", "MMUL", "MP3", "MSX", "NONMUSIC", "ST-00")
  val compo = dirs(3)
  if (competitions.contains(compo)) {
    Some(FujiologyMeta(
      md5 = e.md5,
      authors = Buffer.empty,
      publishers = Buffer.empty,
      album = "",
      year = Some(dirs(1).toInt),
      system = "",
      prodType = "",
    ))
  } else None
}

lazy val metas = (music_metas ++ prods_metas ++ party_metas ++ mags_metas)
