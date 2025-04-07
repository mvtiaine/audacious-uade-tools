// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0
//> using dep net.ruippeixotog::scala-scraper::3.1.0

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._

val oldexotica_path = System.getProperty("user.home") + "/oldexotica/"

case class OldExoticaMeta (
  archive: String,
  md5: String,
  path: String,
  filesize: Int,
  author_handle: String,
  name_source: String,
  info: String,
  year: Option[Int],
)

lazy val oldexotica_mods_by_path = sources.oldexotica.groupBy(_.path.trim.split("/").takeRight(2).mkString("/"))
lazy val oldexotica_mods_by_dir = sources.oldexotica.groupBy(_.path.trim.split("/").takeRight(3).take(2).mkString("/"))

lazy val metas = Files.list(Paths.get(oldexotica_path + "tunes/pages-full/")).toScala(Buffer).par.flatMap(f =>
  val doc = JsoupBrowser().parseFile(f.toFile, "cp1252")
  val rows = doc >> elementList("center table tr[bgcolor=\"#333355\"]")
  val metas = Buffer[OldExoticaMeta]()
  var meta = OldExoticaMeta("", "", "", 0, "", "", "", None)
  var skip = 0
  var n = 1
  rows.foreach(e =>
    val cols = e >> elementList("td")
    cols.foreach(col =>
      if ((col >> elementList("font a")).size > 0) {
        val a = col >> elementList("font a")
        val href = a(0).attr("href")
        val archive = href.trim.split("/").takeRight(2).mkString("/")
        meta = meta.copy(archive = archive)
        skip = 2
      } else if ((col >> elementList("a")).size > 0) {
        val a = col >> elementList("a")
        val href = a(0).attr("href")
        val archive = href.trim.split("/").takeRight(2).mkString("/")
        val file = archive.split("/").last.replace(".lha", "")
        if (file == "Info.txt") {
          skip = 2
        } else {
            meta = meta.copy(archive = archive, path = file + "/" + file)
            n += 1
        }
      } else if (skip > 0) {
        skip -= 1
      } else if (col >> text("font") == "Info.txt") {
        skip = 2
      } else {
        val txt = col >> text("font")
        n match {
          case 1 => meta = meta.copy(path = meta.archive.split("/").last.replace(".lha", "") + "/" + txt.trim)
          case 2 => meta = meta.copy(filesize = txt.trim.toIntOption.getOrElse(0))
          case 3 => meta = meta.copy(author_handle = txt.trim)
          case 4 => meta = meta.copy(name_source = txt.trim)
          case 5 => meta = meta.copy(info = txt.trim)
          case 6 =>
            if (meta.filesize == 0) {
              oldexotica_mods_by_dir.get(meta.path).headOption.foreach { md5s =>
                if (md5s.isEmpty) {
                  System.err.println(s"WARN: oldexotica missing md5s for '${meta.path}' (${meta.archive})")
                }
                md5s.map(_.md5).foreach { md5 => 
                  meta = meta.copy(md5 = md5, year = txt.toIntOption)
                  metas.append(meta)
                }
              }
            } else {
              val md5 = oldexotica_mods_by_path.get(meta.path).headOption.map(_.head.md5)
              if (md5.isEmpty) {
                val name = meta.path.split("/").last.toLowerCase
                if (!name.startsWith("smpl.") &&
                    !name.startsWith("jpns.") &&
                    !name.startsWith("emul.") &&
                    !name.startsWith("amad.") &&
                    !name.startsWith("info.") &&
                    !name.endsWith(".smp") &&
                    !name.endsWith("_replay") &&
                    !name.endsWith(".ins") &&
                    !name.endsWith(".bin") &&
                    !name.endsWith(".s") &&
                    !name.endsWith(".asm") &&
                    !name.endsWith(".pat") &&
                    !name.endsWith(".dat") &&
                    !name.endsWith(".pos") &&
                    !name.endsWith(".mus") &&
                    !name.endsWith(".fx") &&
                    !name.endsWith(".txt") &&
                    !name.endsWith(".readme") &&
                    !meta.path.startsWith("DOCUMENTS")
                ) {
                    System.err.println(s"WARN: oldexotica missing md5 for '${meta.path}' (${meta.archive})")
                }
              } else {
                  meta = meta.copy(md5 = md5.get, year = txt.toIntOption)
                  metas.append(meta)
              }
              meta = OldExoticaMeta(meta.archive, "", "", 0, "", "", "", None)
            }
           case _ => throw new IllegalStateException(s"Unexpected column $n")
        }
        n += 1
        if (n > 6) n = 1
      }
    )
  )
  metas
).distinct.seq

def transformAuthors(meta: OldExoticaMeta): Buffer[String] = {
  var author = meta.author_handle
  if (author.endsWith("N/A") || author.endsWith("?") || author.endsWith("Various")) return Buffer.empty
  if (author.startsWith("(") && author.endsWith(")")) {
    author = author.substring(1, author.length - 1).trim
  }
  if (author.contains(" (") && author.endsWith(")")) {
    author = author.split(" \\(").tail.head.replaceAll("\\)", "").trim
  }
  if (author.split("/").length > 1) {
    author = author.split("/").head.trim
  }
  if (author.split(",| and ").length > 1) {
    author.split(",| and ").map(a =>
      if (a.contains(" (") && a.endsWith(")")) {
        a.split(" \\(").tail.head.replaceAll("\\)", "").trim
      } else {
        if (a.contains(" (")) a.split(" \\(").tail.head.trim
        else a.trim
      }
    ).sorted.toBuffer
  } else if (!author.endsWith("?")) {
    if (author.contains(" (")) {
        author = author.split(" \\(").tail.head.trim
    }
    if (author.startsWith("(") && author.endsWith(")")) {
      author = author.substring(1, author.length - 1).trim
    }
    Buffer(author)
  } else {
    Buffer.empty
  }
}

def transformPublishers(meta: OldExoticaMeta): Buffer[String] = {
  var publisher = meta.info
  if (publisher.endsWith("N/A") ||
      publisher.endsWith("?") ||
      publisher.endsWith("Various") ||
      publisher.endsWith("(Unfinished)"))
      return Buffer.empty
  publisher = publisher.replaceAll(" \\(Unreleased \\?\\)", "")
  publisher = publisher.replaceAll(" \\(Unrel.\\)", "")
  if (publisher.startsWith("Game (")) {
    publisher = publisher.split(" \\(").tail.head.replaceAll("\\)", "").trim
  }
  if (publisher.startsWith("Musicdisk (")) {
    publisher = publisher.split(" \\(").tail.head.replaceAll("\\)", "").trim
  }
  if (publisher.startsWith("Intro (")) {
    publisher = publisher.split(" \\(").tail.head.replaceAll("\\)", "").trim
  }
  if (publisher.startsWith("Advert (")) {
    publisher = publisher.split(" \\(").tail.head.replaceAll("\\)", "").trim
  }
  if (publisher.startsWith("Magazine (")) {
    publisher = publisher.split(" \\(").tail.head.replaceAll("\\)", "").trim
  }
  if (publisher.endsWith("/Falcon")) {
    publisher = publisher.split("/").head
  }
  if (publisher.startsWith("© ")) {
    publisher = publisher.replaceAll("© ", "")
  }
  if (publisher == "For UGA and 17Bit Compo") {
    publisher = "UGA and 17Bit Compo"
  }
  if (publisher.endsWith("?") ||
      publisher == "Game" ||
      publisher == "Games" ||
      publisher == "Demo" ||
      publisher == "Intro" ||
      publisher == "Diskmag" ||
      publisher == "Utility" ||
      publisher == "Cruncher Utility" ||
      publisher == "(Made for unfinished game)" ||
      publisher == "THX Sound System" ||
      publisher == "Music Demo" ||
      publisher == "Advert" ||
      publisher == "Megadrive" ||
      publisher.startsWith("Demo (") ||
      publisher.startsWith("Musicdisk") ||
      publisher.startsWith("Compo") ||
      publisher.startsWith("(Another version of") ||
      publisher.startsWith("Cover - ") ||
      publisher.startsWith("Conv. Of C64 Tune") ||
      publisher.startsWith("Not Rel") ||
      publisher.startsWith("7 Voice") ||
      (publisher.contains("Demo") && publisher.toLowerCase.contains(" song"))) {
    Buffer.empty
  } else if (publisher.contains("/")) {
    publisher.split("/").map(_.trim).sorted.toBuffer
  } else {
    if (publisher.startsWith("(") && publisher.endsWith(")")) {
      publisher = publisher.substring(1, publisher.length - 1).trim
    }
    Buffer(publisher)
  }
}

def transformAlbum(meta: OldExoticaMeta): String = {
  var album = meta.name_source
  if (album.endsWith("N/A") || album.endsWith("?") || album.endsWith("Various")) return ""
  if (album.startsWith("(") && album.contains(") ")) {
    album = album.split("\\) ").tail.head.trim
  }
  if (album.contains(" (") && album.endsWith(")")) {
    album = album.split(" \\(").head.trim
  }
  if (album.contains(" / ")) {
    album = album.split(" / ").head.trim
  }
  if (album == "DTACK Music Comp") {
    album = ""
  }
  album
}
