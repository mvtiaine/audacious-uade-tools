// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0
//> using dep net.ruippeixotog::scala-scraper::3.1.0

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.Using
import scala.util.boundary, boundary.break

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._

import convert._

val wikipedia_path = System.getProperty("user.home") + "/wikipedia/"

def extractYear(dateStr: String): Int = {
  // Try parsing as a simple integer first (year-only like "2003")
  dateStr.toIntOption.getOrElse {
    // Try to extract year from various date formats:
    // "27 Jun 2003", "November 2008", "March 16, 2000"
    // Find any 4-digit number that looks like a year (1900-2999)
    val yearPattern = "\\b([12]\\d{3})\\b".r
    yearPattern.findFirstMatchIn(dateStr)
      .map(_.group(1).toInt)
      .getOrElse(0)
  }
}

case class WikipediaMeta(
  title: String,
  developers: Buffer[String],
  publishers: Buffer[String],
  year: Int,
)

lazy val dos_metas = Files.list(Paths.get(wikipedia_path)).toScala(Buffer)
  .filter(_.getFileName.toString.startsWith("Index_of_DOS_games"))
  .par.flatMap { f =>
    println(s"Processing WIKIPEDIA file: ${f}")
    val doc = JsoupBrowser().parseFile(f.toFile)
    val rows = doc >> elementList("table.wikitable tbody tr")
    
    rows.drop(1).flatMap { row =>
      val cells = row >> elementList("td")
      if (cells.length >= 4) {
        val texts = cells.map(c => (c >> text).trim)

        val meta = WikipediaMeta(
          title = texts(0),
          developers = texts(2).split(",").map(_.trim).to(Buffer),
          publishers = texts(3).split(",").map(_.trim).to(Buffer),
          year = extractYear(texts(1)),
        )
        println(s"WIKIPEDIA DOS META: ${meta}")
        Some(meta)
      } else {
        None
      }
    }
  }.toList

lazy val windows_metas = Files.list(Paths.get(wikipedia_path)).toScala(Buffer)
  .filter(_.getFileName.toString.startsWith("Index_of_Windows_games"))
  .par.flatMap { f =>
    println(s"Processing WIKIPEDIA file: ${f}")
    val doc = JsoupBrowser().parseFile(f.toFile)
    val rows = doc >> elementList("table.wikitable tbody tr")
    
    rows.drop(1).flatMap { row =>
      val cells = row >> elementList("td")
      if (cells.length >= 4) {
        val texts = cells.map(c => (c >> text).trim)

        val meta = WikipediaMeta(
          title = texts(0),
          developers = texts(2).split(",").map(_.trim).to(Buffer),
          publishers = texts(3).split(",").map(_.trim).to(Buffer),
          year = extractYear(texts(1)),
        )
        println(s"WIKIPEDIA WINDOWS META: ${meta}")
        Some(meta)
      } else {
        None
      }
    }
  }.toList

lazy val windows_3x_metas = Files.list(Paths.get(wikipedia_path)).toScala(Buffer)
  .filter(_.getFileName.toString.startsWith("List_of_Windows_3.x_games"))
  .par.flatMap { f =>
    println(s"Processing WIKIPEDIA file: ${f}")
    val doc = JsoupBrowser().parseFile(f.toFile)
    val rows = doc >> elementList("table.wikitable tbody tr")
    
    rows.drop(1).flatMap { row =>
      val cells = row >> elementList("td")
      if (cells.length >= 3) {
        val texts = cells.map(c => (c >> text).trim)

        val meta = WikipediaMeta(
          title = texts(0),
          developers = texts(2).split(",").map(_.trim).to(Buffer),
          publishers = Buffer.empty, // developer/publisher is combined
          year = extractYear(texts(1)),
        )
        println(s"WIKIPEDIA WINDOWS 3.x META: ${meta}")
        Some(meta)
      } else {
        None
      }
    }
  }.toList

lazy val pc_metas = Files.list(Paths.get(wikipedia_path)).toScala(Buffer)
  .filter(_.getFileName.toString.startsWith("List_of_PC_games"))
  .par.flatMap { f =>
    println(s"Processing WIKIPEDIA file: ${f}")
    val doc = JsoupBrowser().parseFile(f.toFile)
    val rows = doc >> elementList("table.wikitable tbody tr")
      
    rows.drop(1).flatMap { row =>
      val cells = row >> elementList("td")
      // Name 	Developer 	Publisher 	Genre(s) 	Operating system(s) 	Date released 
      if (cells.length >= 6) {
        val texts = cells.map(c => (c >> text).trim)
  
        val meta = WikipediaMeta(
          title = texts(0),
          developers = texts(1).split(",").map(_.trim).to(Buffer),
          publishers = texts(2).split(",").map(_.trim).to(Buffer),
          year = extractYear(texts(5)),
        )
        println(s"WIKIPEDIA PC META: ${meta}")
        Some(meta)
      } else {
        None
      }
    }
  }.toList

lazy val free_pc_metas = Files.list(Paths.get(wikipedia_path)).toScala(Buffer)
  .filter(_.getFileName.toString.startsWith("List_of_free_PC_games"))
  .par.flatMap { f =>
    println(s"Processing WIKIPEDIA file: ${f}")
    val doc = JsoupBrowser().parseFile(f.toFile)
    val rows = doc >> elementList("table.wikitable tbody tr")
      
    rows.drop(1).flatMap { row =>
      val cells = row >> elementList("td")
      // Name 	Developer 	Publisher 	Genre(s) 	Operating system(s) 	Date released 	Date free 	Free type 	Metacritic
      if (cells.length >= 6) {
        val texts = cells.map(c => (c >> text).trim)
  
        val meta = WikipediaMeta(
          title = texts(0),
          developers = texts(1).split(",").map(_.trim).to(Buffer),
          publishers = texts(2).split(",").map(_.trim).to(Buffer),
          year = extractYear(texts(5)),
        )
        println(s"WIKIPEDIA FREE PC META: ${meta}")
        Some(meta)
      } else {
        None
      }
    }
  }.toList

lazy val cd32_metas = Files.list(Paths.get(wikipedia_path)).toScala(Buffer)
  .filter(_.getFileName.toString.startsWith("List_of_Amiga_CD32_games"))
  .par.flatMap { f =>
    println(s"Processing WIKIPEDIA file: ${f}")
    val doc = JsoupBrowser().parseFile(f.toFile)
    val rows = doc >> elementList("table.wikitable tbody tr")
      
    rows.drop(1).flatMap { row =>
      val cells = row >> elementList("td")
      // Title[9][10][11][12] 	Genre(s) 	Developer(s)[11][12] 	Publisher(s)[9][11][12] 	Release date(s) 	CD32 version 
      if (cells.length >= 5) {
        val texts = cells.map(c => (c >> text).trim)
  
        val meta = WikipediaMeta(
          title = texts(0),
          developers = texts(2).split(",").map(_.trim).to(Buffer),
          publishers = texts(3).split(",").map(_.trim).to(Buffer),
          year = extractYear(texts(4)),
        )
        println(s"WIKIPEDIA CD32 META: ${meta}")
        Some(meta)
      } else {
        None
      }
    }
  }.toList

lazy val jaguar_metas = Files.list(Paths.get(wikipedia_path)).toScala(Buffer)
  .filter(_.getFileName.toString.startsWith("List_of_Atari_Jaguar_games"))
  .par.flatMap { f =>
    println(s"Processing WIKIPEDIA file: ${f}")
    val doc = JsoupBrowser().parseFile(f.toFile)
    val rows = doc >> elementList("table.wikitable tbody tr")
      
    rows.drop(1).flatMap { row =>
      val title = row >> text("th")
      val cells = row >> elementList("td")
      // Titles[13] 	Developers[13] 	Publishers[13][14] 	NA[15][16][17] 	EU[18] 	Ref.
      if (cells.length >= 3) {
        val texts = cells.map(c => (c >> text).trim)
        // Extract years from all release date columns (NA, EU, and possibly more)
        val releaseYears = (2 until (cells.length - 1)).map(i => extractYear(texts(i))).filter(_ > 0)
        val earliestYear = if (releaseYears.nonEmpty) releaseYears.min else 0
  
        val meta = WikipediaMeta(
          title,
          developers = texts(0).split(",").map(_.trim).to(Buffer),
          publishers = texts(1).split(",").map(_.trim).to(Buffer),
          year = earliestYear,
        )
        println(s"WIKIPEDIA JAGUAR META: ${meta}")
        Some(meta)
      } else {
        None
      }
    }
  }.toList

lazy val wikipediaMetas = ((dos_metas ++ windows_metas ++ windows_3x_metas ++ pc_metas ++ free_pc_metas).map(m =>
  MetaData(
    hash = "",
    authors = Buffer.empty,
    album = m.title.trim,
    publishers = (m.developers ++ m.publishers).sorted.distinct.toBuffer,
    year = m.year,
    _type = "Game",
    _platform = "PC",
  )
) ++ cd32_metas.map(m =>
  MetaData(
    hash = "",
    authors = Buffer.empty,
    album = m.title.trim,
    publishers = (m.developers ++ m.publishers).sorted.distinct.toBuffer,
    year = m.year,
    _type = "Game",
    _platform = "Amiga",
  )
) ++ jaguar_metas.map(m =>
  MetaData(
    hash = "",
    authors = Buffer.empty,
    album = m.title.trim,
    publishers = (m.developers ++ m.publishers).sorted.distinct.toBuffer,
    year = m.year,
    _type = "Game",
    _platform = "Atari",
  )
))
.map(m => if (m.publishers.forall(_ == m.album)) m.copy(publishers = Buffer.empty) else m) // XXX
.toSet.seq

for (m <- wikipediaMetas) {
  println(s"WIKIPEDIA FINAL META: ${m}")
}
