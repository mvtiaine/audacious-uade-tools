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

val fujiology_xlsx = System.getProperty("user.home") + "/fujiology/fujiology_archive_2_9_7.xlsx"
lazy val fujiology_by_filename = sources.fujiology.groupBy(_.path.split("/").last.toLowerCase)
/* 
lazy val fujiology_by_path = sources.fujiology.flatMap { entry =>
  val path = entry.path.toLowerCase
  val parts = path.split("/").dropRight(1)
  val pathFixed = if (parts.length >= 2 && parts.last == parts(parts.length - 2)) {
    parts.dropRight(1)
  } else {
    parts
  }
  val path2 = pathFixed.mkString("/").toLowerCase
  Seq(
    (path, entry),
    (path2, entry),
  )
}.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
 */
// Group	Prod	Type	Demozoo	Pouet	Atarimania/Stonish	Filename	Music (Chip) / Composer	Music (Digi) / Composer	Release date	Info
case class FujiologyRow (
  group: String,
  prod: String,
  `type`: String,
  demozoo: String,
  pouet: String,
  atarimania_stonish: String,
  filename: String,
  music_chip_composer: String,
  music_digi_composer: String,
  release_date: String,
  info: String
)
// // Group	Prod	Type	Demozoo	Pouet	Atarimania	Filename	Music (Chip) / Composer	Music (Digi) / Composer	Musicentry	Info		
case class PlatformRow (
  group: String,
  prod: String,
  `type`: String,
  demozoo: String,
  pouet: String,
  atarimania: String,
  filename: String,
  music_chip_composer: String,
  music_digi_composer: String,
  musicentry: String,
  info: String
)

// Composer	Songname	Demozoo	Filename	Format	System	Prod	Crew	Prodlink	Info	Foldername					
case class MusicRow (
  composer: String,
  songname: String,
  demozoo: String,
  filename: String,
  format: String,
  system: String,
  prod: String,
  crew: String,
  prodlink: String,
  info: String,
  foldername: String
)

case class FujiologyMeta (
  md5: String,
  authors: Buffer[String],
  publishers: Buffer[String],
  album: String,
  year: Option[Int],
  system: String,
)

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

lazy val music_metas = {
  val metas = Buffer[FujiologyMeta]()
  val by_filename = mutable.Map.from(fujiology_by_filename)
  val file = new FileInputStream(new File(fujiology_xlsx))
  val workbook = new XSSFWorkbook(file)
  val sheet = workbook.getSheet("MUSIC")
  var prevfolder = ""
  var prevprod = ""
  val rows = sheet.iterator()
  rows.next()
  while (rows.hasNext()) {
    val row = rows.next()
    if (row.getLastCellNum() >= 4) {
      val composer = getCellString(row, 0)
      val songname = getCellString(row, 1)
      val demozoo = getCellString(row, 2)
      val filenames = getCellString(row, 3).toLowerCase.split(",").map(_.trim).filterNot(_.isEmpty)
      val system = getCellString(row, 5)
      val prod = getCellString(row, 6)
      val crew = getCellString(row, 7)
      var foldername = getCellString(row, 10)
      if (prod != prevprod) {
        prevprod = prod
        prevfolder = foldername
      }
      if (foldername.isEmpty) {
        foldername = prevfolder
      }
      val composers = composer
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
      val publishers = crew
        .split("/| & ")
        .map(_.trim)

      filenames.foreach { filename =>
        var entries = by_filename.getOrElse(filename, Seq())
          .filter(_.path.startsWith("MUSIC/"))

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
            entries = entries.filter(e => normalize(e.path).contains(s"/${normalize(foldername)}/"))
          }
          if (!entries.isEmpty && entries.forall(_.md5 == entries.head.md5)) {
            entries = Seq(entries.head)
          }
          if (entries.size > 1 && !composers.isEmpty) {
            entries = entries.filter(e => normalize(e.path).contains(s"/${normalize(composers.head).take(8)}/"))
          }
          if (!entries.isEmpty && entries.forall(_.md5 == entries.head.md5)) {
            entries = Seq(entries.head)
          }
          if (entries.isEmpty) {
            System.err.println(s"WARN: Fujiology no matches after filtering for '${filename}' composer '${composer}' folder '${foldername}': ${origEntries}")
          } else if (entries.size > 1) {
            System.err.println(s"WARN: Fujiology multiple matches for '${filename}' composer '${composer}' folder '${foldername}': ${origEntries}")
          }
        }
        if (entries.size == 1) {
          val entry = entries.head
          if (metas.exists(m => m.md5 == entry.md5)) {
            System.err.println(s"WARN: Fujiology duplicate ignored: ${entry} vs ${metas.filter(_.md5 == entry.md5).head}")
          } else {
            metas += FujiologyMeta(
              md5 = entry.md5,
              authors = composers.filterNot(_.isEmpty).sorted.distinct.toBuffer,
              publishers = publishers.filterNot(_.isEmpty).sorted.distinct.toBuffer,
              album = prod,
              year = None,
              system,
            )
          }
          // TODO check
          // TODO process rows twice/multiple times (but only "ambiguous/leftovers")
          by_filename.update(filename, by_filename(filename).filterNot(_.md5 == entry.md5))
        }
      }
    }
  }
  file.close()
  metas
}

// TODO heuristics for conflicts
// TODO check txt files?
lazy val metas = music_metas
