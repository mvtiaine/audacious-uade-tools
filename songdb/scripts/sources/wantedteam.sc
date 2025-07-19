// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.Using

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._

val wantedteam_path = System.getProperty("user.home") + "/wantedteam/"

case class WantedTeamMeta (
  md5: String,
  path: String,
  filesize: Int,
  authors: Buffer[String],
  album: String,
  publishers: Buffer[String],
  year: Option[Int]
)

lazy val wantedteam_customs_by_path =
  sources.wantedteam.filter(_.path.startsWith("customs/"))
  .groupBy(_.path.split("/").drop(1).take(1).mkString)

lazy val customstxt = Paths.get(wantedteam_path + "customs/OnePlayControl/WT_Customs.txt").toFile
lazy val customs = Using(scala.io.Source.fromFile(customstxt)(using scala.io.Codec.ISO8859))(_.getLines.toBuffer.drop(3).par.flatMap { line =>
  val parts = line.split("\\s+")
  val path = parts(0).replace(".lha","").replace(".lzx","")
  val size = parts(1).toIntOption.getOrElse(0)
  val ver = parts(2).toInt
  val title = parts.drop(3).mkString(" ")
    .replace(" (c1) "," (c) ")
    .replace(" (c) (c) "," (c) ")

  var album: String = ""
  var year: Option[Int] = None
  var publishers = Buffer.empty[String]

  if (title.endsWith(" year)")) {
    album = title.substring(0, title.indexOf(" ("))
    year = title.substring(title.indexOf(" (")+2, title.indexOf(" (")+6).toIntOption
  } else if (title.contains(" (c) by ")) {
    album = title.substring(0, title.indexOf(" (c) by "))
    year = title.substring(title.indexOf(" (c) by ")+8, title.indexOf(" (c) by ")+12).toIntOption
    publishers = title.substring(title.indexOf(" (c) by ")+12).split("/").map(_.trim).toBuffer
  } else if (title.contains(" by ") && (title.contains(" (c) 1") || title.contains(" (c) 2"))) {
    album = title.split(" \\(c\\) ").head
    year = title.split(" \\(c\\) ").drop(1).head.split(" by ").head.trim.toIntOption
    publishers = title.split(" \\(c\\) ").drop(1).head.split(" by ").drop(1).head.trim.split("/").toBuffer
  } else if (title.contains(" (c) 1") || title.contains(" (c) 2")) {
    album = title.split(" \\(c\\) ").head
    year = title.split(" \\(c\\) ").drop(1).head.substring(0, 4).toIntOption
    publishers = title.split(" \\(c\\) ").drop(1).head.substring(5).split("/").map(_.trim).toBuffer
  } else if (title.contains(" (c) ")) {
    album = title.substring(0, title.indexOf(" (c) "))
    publishers = title.substring(title.indexOf(" (c) ")+5).split("/").map(_.trim).toBuffer
  } else if (title.endsWith(" (?) year")) {
    album = title.substring(0, title.indexOf(" (?) year")-5)
    year = title.substring(title.indexOf(" (?) year")-4, title.indexOf(" (?) year")).toIntOption
  } else {
    System.err.println(s"WARN: wantedteam customs ignoring metadata for $path")
  }
  val entries = wantedteam_customs_by_path.getOrElse(path, Seq.empty)
  if (entries.isEmpty) {
    System.err.println(s"WARN: wantedteam customs missing md5s for $path")
  }
  entries.map(e =>
    WantedTeamMeta(e.md5, e.path, size, Buffer(), album, publishers.sorted, year)
  )
}).get.distinct.seq

lazy val wantedteam_examples_by_path =
  sources.wantedteam.filter(_.path.startsWith("examples/"))
  .groupBy(_.path.split("/").drop(1).take(1).mkString)

lazy val exampleshtml = Paths.get(wantedteam_path + "examples.html").toFile
lazy val examples = {
  val doc = JsoupBrowser().parseFile(exampleshtml)
  val rows = doc >> elementList("li")
  rows.par.flatMap(r =>
    var txt = r.text.trim
    val path = txt.split(" ").head.replace(".lha","").replace(".lzx","")
    val filesize = txt.split(" ").drop(1).head.replace("(","").replace(" bytes)","").toIntOption.getOrElse(0)
    txt = txt.split(" - ").tail.mkString(" - ").trim

    var album: String = ""
    var authors = Buffer.empty[String]
    var publishers = Buffer.empty[String]
    var year: Option[Int] = None

    if (txt.startsWith("different modules converted to ") ||
        txt.startsWith("source of ") ||
        txt.startsWith("all modules from the games ") ||
        txt.contains("module called ")
    ) {
      // no available metadata or ambiguous
    } else if (
        txt.startsWith("all modules from demo of ") ||
        txt.startsWith("all modules from the demo of ") ||
        txt.startsWith("all modules from the demos of ") ||
        txt.startsWith("module from the demo of ")
    ) {
      album = txt.substring(txt.indexOf("\""), txt.indexOf("\"", txt.indexOf("\"")+1)).replace("\"","").trim + " (demo)"
      if (txt.matches(".* composed [BbRr]y .*")) {
        authors = txt.substring(txt.indexOf(" composed ")+12).split("\\.").head.split(",|&").map(_.trim).sorted.toBuffer
      } else if (txt.contains(" composed ")) {
        authors = txt.substring(txt.indexOf(" composed ")+10).split("\\.").head.split(",|&").map(_.trim).sorted.toBuffer
      } else if (txt.contains(" by ")) {
        publishers = txt.substring(txt.indexOf(" by ")+4, txt.indexOf(".")).split(",|&").map(_.trim).sorted.toBuffer    
      }

    } else if (
        txt.startsWith("example ") ||
        txt.startsWith("module composed ")
    ) {
      if (txt.matches(".* composed [BbRr]y .*")) {
        authors = txt.substring(txt.indexOf(" composed ")+12).split("\\.").head.split(",|&").map(_.trim).sorted.toBuffer
      } else if (txt.contains(" composed ")) {
        authors = txt.substring(txt.indexOf(" composed ")+10).split("\\.").head.split(",|&").map(_.trim).sorted.toBuffer
      }

    } else if (
        txt.startsWith("all modules from the demo ") ||
        txt.startsWith("all modules from the beta version of ") ||
        txt.startsWith("all modules from the disk mag ") ||
        txt.startsWith("all modules from the game ") ||
        txt.startsWith("all modules from the music disk ") ||
        txt.startsWith("all modules from/for the game ") ||
        txt.startsWith("all modules from/for the games ") ||
        txt.startsWith("all modules for the game ") ||
        txt.startsWith("all music ") ||
        txt.startsWith("missing module from the game ") ||
        txt.startsWith("module for the game ") ||
        txt.startsWith("module from the cruncher ") ||
        txt.startsWith("module from the demo ") ||
        txt.startsWith("module from the game ") ||
        txt.startsWith("module from the games ") ||
        txt.startsWith("module from the intro ") ||
        txt.startsWith("module from the preview of ") ||
        txt.startsWith("module(s) from the game ") ||
        txt.startsWith("modules from/for the game ") ||
        txt.startsWith("music from the demo ")
    ) {
      album = txt.substring(txt.indexOf("\""), txt.indexOf("\"", txt.indexOf("\"")+1)).replace("\"","").trim
      if (txt.contains("beta version")) {
        album += " (beta)"
      } else if (txt.contains("the preview of")) {
        album += " (preview)"
      } else if (txt.contains("(remastered version)")) {
        album += " (remastered)"
      } else if (txt.contains("(first version)")) {
        album += " (1st version)"
      } else if (txt.contains("(second version)")) {
        album += " (2nd version)"
      } else if (txt.contains("(unknown version)")) {
        album += " (unknown version)"
      } else if (txt.contains("(US version)")) {
        album += " (US)"
      } else if (txt.contains("(AGA version)")) {
        album += " (AGA)"
      } else if (txt.contains("(CD32 version)")) {
        album += " (CD32)"
      } else if (txt.contains("(Amiga version)")) {
        album += " (Amiga)"
      } else if (txt.contains("(Atari ST version)")) {
        album += " (Atari ST)"
      } else if (txt.contains("(Atari STE version)")) {
        album += " (Atari STE)"
      } else if (txt.contains("(PC version)")) {
        album += " (PC)"
      } else if (txt.contains("(1990 version)")) {
        album += " (1990)"
        year = Some(1990)
      } else if (txt.contains("(1989 version)")) {
        album += " (1989)"
        year = Some(1989)
      }
      if (txt.matches(".* composed [BbRr]y .*")) {
        authors = txt.substring(txt.indexOf(" composed ")+12).split("\\.").head.split(",|&").map(_.trim).sorted.toBuffer
      } else if (txt.contains(" composed ")) {
        authors = txt.substring(txt.indexOf(" composed ")+10).split("\\.").head.split(",|&").map(_.trim).sorted.toBuffer
      } else if (txt.contains(" by ")) {
        publishers = txt.substring(txt.indexOf(" by ")+4, txt.indexOf(".")).split(",|&").map(_.trim).sorted.toBuffer
      }

    } else if (
        txt.startsWith("module from a ") ||
        txt.startsWith("modules from ")
    ) {
      publishers = Buffer(txt.substring(txt.indexOf("\""), txt.indexOf("\"", txt.indexOf("\"")+1)).replace("\"","").trim)

    } else if (
        txt.startsWith("all modules from ") ||
        txt.startsWith("module from ")
    ) {
      if (txt.startsWith("all modules from the Ringard")) {
        album = "Megademo"
        publishers = Buffer("Ringard")

      } else if (txt.endsWith("Sierra demo.")) {
        publishers = Buffer("Sierra")

      } else if (txt.contains("\"")) {
        album = txt.substring(txt.indexOf("\""), txt.indexOf("\"", txt.indexOf("\"")+1)).replace("\"","").trim
        if (txt.matches(".* composed [BbRr]y .*")) {
          authors = txt.substring(txt.indexOf(" composed ")+12).split("\\.").head.split(",|&").map(_.trim).sorted.toBuffer
        } else if (txt.contains(" composed ")) {
          authors = txt.substring(txt.indexOf(" composed ")+10).split("\\.").head.split(",|&").map(_.trim).sorted.toBuffer
        } else if (txt.contains(" by ")) {
          publishers = txt.substring(txt.indexOf(" by ")+4, txt.indexOf(".")).split(",|&").map(_.trim).sorted.toBuffer
        }
        if (txt.endsWith(" intro.")) {
          album += " intro"
        }
        if (txt.endsWith(" intros.") || album == "Software 2000") {
            publishers = Buffer(album)
            album = "";
        }
      } else {
        // no available metadata or ambiguous
      }
    } else {
      // no available metadata or ambiguous
    }

    val entries = wantedteam_examples_by_path.getOrElse(path, Seq.empty)
    if (entries.isEmpty && !txt.startsWith("source of")) {
        System.err.println(s"WARN: wantedteam examples missing md5s for $path")
    }
    if (!album.isEmpty || !authors.isEmpty || !publishers.isEmpty || year.isDefined) {
      entries.map(e =>
        WantedTeamMeta(e.md5, e.path, filesize, authors, album, publishers, year)
      )
    } else {
      Seq.empty
    }
  )
}.distinct.seq

lazy val wantedteam_rips_by_path =
  sources.wantedteam.filter(_.path.startsWith("rips/"))
  .groupBy(_.path.split("/").drop(1).take(1).mkString)

lazy val ripshtml = Paths.get(wantedteam_path + "rips.html").toFile
lazy val rips = {
  val doc = JsoupBrowser().parseFile(ripshtml)
  val rows = doc >> elementList("li")
  rows.par.flatMap(r =>
    var txt = r.text.trim
    val path = txt.split(" ").head.replace(".lha","").replace(".lzx","")
    val filesize = txt.split(" ").drop(1).head.replace("(","").replace(" bytes)","").toIntOption.getOrElse(0)
    txt = txt.split(" - ").tail.mkString(" - ").trim

    var album: String = ""
    var authors = Buffer.empty[String]
    var publishers = Buffer.empty[String]
    var year: Option[Int] = None

    if (txt.contains("\"")) {
      album = txt.substring(txt.indexOf("\""), txt.indexOf("\"", txt.indexOf("\"")+1)).replace("\"","").trim
      if (txt.contains("(AGA version)")) {
        album += " (AGA)"
      } else if (txt.contains("(Atari ST version)")) {
        album += " (Atari ST)"
      }
      if (txt.contains("\" by Confusion")) {
        publishers = Buffer("Confusion")
      }
      if (txt.matches(".* composed [BbRr]y .*")) {
        authors = txt.substring(txt.indexOf(" composed ")+12).split("\\.").head.split(",|&").map(a =>
          if (a.contains("(") && a.contains(")")) {
            a.split("\\(").tail.head.replaceAll("\\)", "").trim
          } else a.trim
        ).sorted.toBuffer
        } else if (txt.contains(" composed ")) {
        authors = txt.substring(txt.indexOf(" composed ")+10).split("\\.").head.split(",|&").map(a =>
          if (a.contains("(") && a.contains(")")) {
            a.split("\\(").tail.head.replaceAll("\\)", "").trim
          } else a.trim
        ).sorted.toBuffer
      } else if (txt.contains(" by ")) {
        publishers = txt.substring(txt.indexOf(" by ")+4, txt.indexOf(".")).split(",|&").map(_.trim).sorted.toBuffer
      }
    } else if (txt.trim == "all SoundTracker modules from the Random Access music disk.") {
      publishers = Buffer("Random Access")
    } else if (txt.trim == "SoundTracker modules from the Ackerlight intros.") {
      publishers = Buffer("Ackerlight")
    } else if (txt.trim == "EaglePlayer v1.0 intro (UNIC Tracker) music.") {
      album = "EaglePlayer v1.0 intro"
    } else if (txt.trim.endsWith("Ron Klaren module.")) {
      authors = Buffer("Ron Klaren")
    } else {
      // no available metadata or ambiguous
    }
    val entries = wantedteam_rips_by_path.getOrElse(path, Seq.empty)
    if (entries.isEmpty) {
        System.err.println(s"WARN: wantedteam rips missing md5s for $path")
    }
    if (!album.isEmpty || !authors.isEmpty || !publishers.isEmpty || year.isDefined) {
      entries.map(e =>
        WantedTeamMeta(e.md5, e.path, filesize, authors, album, publishers, year)
      )
    } else {
      Seq.empty
    }
  )
}.distinct.seq

lazy val metas = (customs ++ examples ++ rips).distinct
