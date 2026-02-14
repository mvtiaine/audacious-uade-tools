// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0
//> using dep net.ruippeixotog::scala-scraper::3.1.0
//> using dep org.apache.commons:commons-lang3:3.20.0

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

import org.apache.commons.text.WordUtils

val amp_path = System.getProperty("user.home") + "/sources/AMP/"

case class AMPMod (
  id: Int,
  md5: String,
  path: String,
  filesize: Int,
)

lazy val amp_by_path = sources.amp.groupBy(_.path.toLowerCase)

lazy val amp_mods = Files.list(Paths.get(amp_path + "downmod/")).toScala(Buffer).par.map(f =>
  val loc = Using(scala.io.Source.fromFile(f.toFile())(using scala.io.Codec.UTF8))( _.getLines.find(_.startsWith("location:"))).get
  if (loc.isDefined) {
    val url = loc.get.replace("location: ","")
    val path = java.net.URLDecoder.decode(url,"UTF-8").replaceAll("http[s]?://amp.dascene.net/modules/","")
    if (amp_by_path.contains(path.toLowerCase)) {
      val e = amp_by_path(path.toLowerCase).head
      Some(AMPMod(f.toString().split("=").last.toInt, e.md5, path, e.filesize))
    } else None
  } else None
).flatten.seq

case class AMPMeta (
  md5: String,
  path: String,
  filesize: Int,
  extra_authors: List[String],
  album: String,
  _type: String,
)

lazy val amp_mods_by_id = amp_mods.groupBy(_.id)

val seenIds = scala.collection.mutable.Set[Int]()
lazy val _metas = Files.list(Paths.get(amp_path + "detail/")).toScala(Buffer).par.map(f =>
  val doc = JsoupBrowser().parseFile(f.toFile)
  val data = doc >> elementList("#result")
  if (data.length > 1) {
    val bar = data(1) >> elementList("table tbody tr[class^=\"tr\"]")
    val ids = bar >> attrs("href")("td a[href^=\"downmod.php\"]")
    val authors = bar >> texts("td a[href^=\"detail.php\"]")
    ids.lazyZip(authors).filterNot(_._1.isEmpty).flatMap({case (idlink, authors) =>
      val id = idlink.head.trim.split("=").last.toInt
      if (!seenIds.contains(id) && amp_mods_by_id.contains(id)) {
        seenIds += id
        val e = amp_mods_by_id(id).head
        val extra_authors = authors.toList.filterNot(a =>
          Seq("!to be deleted!","Unknown Composers").contains(a)) // XXX
        val filename = e.path.split("/").last
        var album =
          if (filename.matches("^\\w+\\.\\([a-zA-Z0-9].*\\).*"))
            filename.split("\\.\\(").last
              .replaceAll("\\).*","")
              .replaceAll("\\(","")
              .replace("_"," ")
              .replaceAll(" \\[DFC\\]$","") // ???
              .replaceAll(" - DFC$","") // ???
              .replaceAll(" DFC$","") // ???
              .trim
          else ""
        val format = filename.split("\\.").head
        // XXX
        var filtered = if (album.length > 1) album else ""
        filtered = if (filtered.contains(" ") && filtered.replace(" ","").toIntOption.isDefined) "" else filtered
        filtered = if (filtered == format || extra_authors.exists(_.contains(filtered))) "" else filtered
        filtered = if (Seq(
          "16 Betha 362","Big Jim","Crim","DJB","Gammis","Impulse","Jape","Liam the Lemming","Logos","Micken","Mystical",
          "SH3","Statix","Tense","The Hooligan","Turtle","WOTW","X-Ball"
        ).exists(a => extra_authors.contains(a))) "" else filtered
        filtered = if (Seq("Look Back in").contains(filtered)) "" else filtered
        // assume all album names refer to games
        Some(AMPMeta(e.md5, e.path, e.filesize, extra_authors, filtered, if (filtered.isEmpty) "" else "Game"))
      } else None
    })
  } else Iterable.empty[AMPMeta]
).flatten.distinct.seq

lazy val _byAlbum: Map[String, Buffer[AMPMeta]] = _metas.filter(_.album.nonEmpty).toBuffer.groupBy(_.album)
lazy val metas = _metas.map(meta =>
  var _meta = if (meta.album.nonEmpty) {
    val m = _byAlbum.getOrElse(meta.album, Buffer.empty).filter(_.extra_authors.exists(a => meta.extra_authors.contains(a)))
    val p = meta.path.split("/").last
    val f = p.split("\\.").head
    if (((m.size == 1 && p == s"${f}.(${meta.album}).gz") || Character.isLowerCase(meta.album.charAt(0))) && !Seq(
        "Bigyo","Darryl Sloan","Laura Shigihara","Max","Mike Anderton","Nightshade","Simon Speight","Unison"
      ).exists(a => meta.extra_authors.contains(a))
    ) {
      meta.copy(album = "", _type = "")
    } else meta
  } else meta
  _meta =
    if (!_meta.album.isEmpty && _meta.album.toLowerCase == _meta.album) _meta.copy(album = WordUtils.capitalize(_meta.album))
    else _meta
  _meta = if (_meta.album.endsWith(" Aga")) _meta.copy(album = _meta.album.replaceAll(" Aga$", " AGA")) else _meta
  _meta
).distinct.seq
