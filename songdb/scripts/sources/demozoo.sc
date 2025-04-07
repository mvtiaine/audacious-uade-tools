// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.Try
import scala.util.Using

import oldexotica.metas

enum Precision:
  case UNKNOWN, YEAR, MONTH, DATE

case class DemozooMeta (
  id: Int,
  prodId: Option[Int],
  modDate: String,
  modDatePrecision: Precision,
  prodDate: String,
  prodDatePrecision: Precision,
  modPlatform: String,
  prodPlatforms: Seq[String],
  prod: String,
  authors: Seq[String],
  modPublishers: Seq[String],
  prodPublishers: Seq[String],
  //imageUrls: Seq[String],
  party: Option[String],
  partyDate: Option[String],
  partyDatePrecision: Option[Precision],
)

lazy val ml_by_path = sources.modland.groupBy(_.path.toLowerCase)
lazy val aminet_by_path = sources.aminet.groupBy(_.path.split("/").take(3).mkString("/").toLowerCase)
lazy val demozoo_leftovers_by_path = sources.demozoo_leftovers.groupBy(_.path.toLowerCase)
lazy val modarchive_by_id = sources.demozoo_leftovers
  .filter(_.path.startsWith("api.modarchive.org")).groupBy(_.path.split("/").take(2).last)
lazy val wantedteam_by_path = sources.wantedteam.groupBy(_.path.split("/").take(2).mkString("/").toLowerCase)
lazy val unexotica_by_path = sources.unexotica.groupBy(_.path.split("/").take(3).mkString("/").toLowerCase)
lazy val oldexotica_by_archive = oldexotica.metas.groupBy(_.archive.toLowerCase)

lazy val metas = Using(scala.io.Source.fromFile("sources/demozoo.tsv"))(_.getLines.toSeq.par.flatMap(line =>
  def split(s: String) = s.replaceFirst("\\{","").replaceAll("\\}$","").split(",").filterNot(_ == "NULL")
  def trim(s: String) = s.trim.replaceFirst("\"","").replaceAll("\"$","")
  def precision(s: String) = s match {
    case "y" => Precision.YEAR
    case "m" => Precision.MONTH
    case "d" => Precision.DATE
    case _ => Precision.UNKNOWN
  }

  val l = line.split("\t")
  val id = l(0).toInt
  val prodId = l(1).toIntOption
  val modDate = l(2)
  val modDatePrecision = precision(l(3))
  val prodDate = l(4)
  val prodDatePrecision = precision(l(5))
  val modPlatform = l(6)
  val prodPlatforms = split(l(7)) map trim
  val prod = l(8)
  val linkClass = l(9)
  val url = l(10)
  val authors = split(l(11)) map trim
  val modPublishers = split(l(12)) map trim
  val prodPublishers = split(l(13)) map trim
  //val imageUrls = split(l(14))
  val party = if (l.length > 14) Some(l(14)) else None
  val partyDate = if (l.length > 15) Some(l(15)) else None
  val partyDatePrecision = if (l.length > 16) Some(precision(l(16))) else None

  val meta = DemozooMeta(id, prodId, modDate, modDatePrecision, prodDate, prodDatePrecision,
    modPlatform, prodPlatforms.toSeq, prod, authors.toSeq, modPublishers.toSeq, prodPublishers.toSeq, // imageUrls.toSeq,
    party, partyDate, partyDatePrecision)

  def findArchive(archivePath: String) = {
    val parts = archivePath.split("/").toSeq
    val path = (parts.dropRight(1) :+ parts.last
      .replaceAll("\\.(dms|adf|lha|lzh|lzx|zip|rar|7z|arj|tgz|tar\\.gz|tar\\.bz2|tar\\.xz|tar)$",""))
      .mkString("/")
      
    val matches = demozoo_leftovers_by_path.filterKeys(_.startsWith(path))
    if (matches.isEmpty) {
      Buffer.empty[String]
    } else {
      var entries = matches.values.flatten
      if (entries.size > 1) {
        System.err.println("WARN: demozoo leftover archive " + archivePath + " - multiple entries - " + entries.mkString(", "))
      }
      entries.map(_.md5).toBuffer.distinct
    }
  }

  def findLeftovers(path: String) =
    if (demozoo_leftovers_by_path.contains(path)) {
      val md5 = demozoo_leftovers_by_path(path).head.md5
      Buffer((md5, meta))
    } else if (findArchive(path).nonEmpty) {
      val md5s = findArchive(path)
      if (md5s.size > 1) {
        md5s.map(md5 => (md5, meta.copy(authors = Seq.empty)))
      } else {
        md5s.map(md5 => (md5, meta))
      }
    } else Buffer.empty

  // non-url links
  if (linkClass == "AmigascneFile") {
    val path = "ftp.amigascne.org/pub/amiga" + url.toLowerCase
    findLeftovers(path)
  } else if (linkClass == "FujiologyFile") {
    val path = "ftp.untergrund.net/users/ltk_tscc/fujiology" + url.toLowerCase
    findLeftovers(path)
  } else if (linkClass == "ModarchiveModule") {
    if (modarchive_by_id.contains(url)) {
      val md5 = modarchive_by_id(url).head.md5
      Buffer((md5, meta))
    } else Buffer.empty
  } else if (linkClass == "ModlandFile" && url.startsWith("/pub/modules/")) {
    val path = url.replaceFirst("/pub/modules/", "").replace("//","/").toLowerCase
    if (ml_by_path.contains(path)) {
      val md5 = ml_by_path(path).head.md5
      Buffer((md5, meta))
    } else Buffer.empty
  } else if (linkClass == "PaduaOrgFile") {
    val path = "ftp.padua.org/pub/c64" + url.toLowerCase
    findLeftovers(path)
  } else if (linkClass == "SceneOrgFile") {
    val path = "files.scene.org/get" + url.toLowerCase
    findLeftovers(path)
  } else if (linkClass == "UntergrundFile") {
    val path = "ftp.untergrund.net" + url.toLowerCase
    findLeftovers(path)
  } else if (linkClass == "WaybackMachinePage") {
    val path = "web.archive.org/web/" + url.toLowerCase
    findLeftovers(path)
  // embedded sources
  } else if (url.contains("://amp.dascene.net/downmod.php?index=")) {
    val id = url.replaceAll("&application=AMP","").split("=").last.toInt
    if (amp.amp_mods_by_id.contains(id)) {
      val md5 = amp.amp_mods_by_id(id).head.md5
      Buffer((md5, meta))
    } else Buffer.empty
  } else if (url.contains("://amp.dascene.net/modules/")) {
    // url should have been decoded already
    val path = url
      .replaceAll("http[s]?://amp.dascene.net/modules/","")
      .replace("//","/")
      .replace(".gz","").toLowerCase
    if (amp.amp_by_path.contains(path)) {
      val md5 = amp.amp_by_path(path).head.md5
      Buffer((md5, meta))
    } else Buffer.empty
  } else if (url.contains("://aminet.net/")) {
    val path = url
      .replaceAll("http[s]?://aminet.net/package/","")
      .replaceAll("http[s]?://aminet.net/","").toLowerCase
      .replace("//","/")
      .replace(".lzx","")
      .replace(".lha","")
    if (aminet_by_path.contains(path)) {
      val entries = aminet_by_path(path)
      if (entries.size > 1) {
        System.err.println("WARN: aminet path " + path + " - multiple entries - " + entries.mkString(", "))
        entries.map(e => (e.md5, meta.copy(authors = Seq.empty)))
      } else {
        entries.map(e => (e.md5, meta))
      }
    } else Buffer.empty
  } else if (url.contains("://wt.exotica.org.uk/files/")) {
    val path = url.replaceAll("http[s]?://wt.exotica.org.uk/files/","").toLowerCase
      .replace("//","/")
      .replace(".lzx","")
      .replace(".lha","")
    if (wantedteam_by_path.contains(path)) {
      val md5 = wantedteam_by_path(path).head.md5
      Buffer((md5, meta))
    } else None
  } else if (url.contains("://files.exotica.org.uk/?file=exotica/media/audio/UnExoticA/") ||
             url.contains("://www.exotica.org.uk/download.php?file=media/audio/UnExoticA/") ||
             url.contains("://www.exotica.org.uk/tunes/archive/Authors/")
  ) {
    val path = url
      .replaceAll("http[s]?://files.exotica.org.uk/\\?file=exotica/media/audio/UnExoticA/","")
      .replaceAll("http[s]?://www.exotica.org.uk/download.php\\?file=media/audio/UnExoticA/", "")
      .replaceAll("http[s]?://www.exotica.org.uk/tunes/archive/Authors/", "")
      .replace("//","/")
      .toLowerCase
      .replace(".lzx","")
      .replace(".lha","")
    if (unexotica_by_path.contains(path)) {
      val entries = unexotica_by_path(path)
      if (entries.size > 1) {
        System.err.println("WARN: unexotica path " + path + " - multiple entries - " + entries)
        entries.map(e => (e.md5, meta.copy(authors = Seq.empty)))
      } else {
        entries.map(e => (e.md5, meta))
      }
    } else Buffer.empty
  } else if (url.contains("://www.exotica.org.uk/tunes/archive/") ||
             url.contains("://old.exotica.org.uk/tunes/archive/")
    ) {
      val archive = url
        .replaceAll("http[s]?://www.exotica.org.uk/tunes/archive/","")
        .replaceAll("http[s]?://old.exotica.org.uk/tunes/archive/","")
        .replace("//","/")
        .toLowerCase
      if (oldexotica_by_archive.contains(archive)) {
        val entries = oldexotica_by_archive(archive)
        if (entries.size > 1) {
          System.err.println("WARN: oldexotica path " + archive + " - multiple entries - " + entries)
          entries.map(e => (e.md5, meta.copy(authors = Seq.empty)))
        } else {
          entries.map(e => (e.md5, meta))
        }
      } else Buffer.empty
  // leftovers
  } else {
    val path = url .replaceAll("http[s]?://","").replace("//","/").toLowerCase
    findLeftovers(path)
  }

).distinct.toBuffer).get.groupBy(_._1).flatMap({case (md5, metas_) =>
  val metas =
    if (metas_.exists(_._2.authors.nonEmpty)) metas_.filter(_._2.authors.nonEmpty)
    else metas_
  var best: Option[DemozooMeta] = None
  if (metas.size == 1) {
    val meta = metas.head._2
    if (meta.prodId.isEmpty) {
      best = Some(meta)
    } else if (meta.prodDatePrecision.ordinal >= Precision.MONTH.ordinal && meta.modDatePrecision.ordinal >= Precision.MONTH.ordinal &&
               meta.prodDate.substring(0,7) <= meta.modDate.substring(0,7)) {               
      best = Some(meta)
    }
  } else for (meta <- metas.map(_._2)) {
    if (best.isDefined) {
    } else if (meta.modDate == meta.prodDate && meta.modDatePrecision == meta.prodDatePrecision) {
      best = Some(meta)
    } else if (meta.modDate == meta.partyDate.getOrElse("") &&
               meta.modDatePrecision == meta.partyDatePrecision.getOrElse(Precision.UNKNOWN)) {
      best = Some(meta)
    }
  }
  
  if (!best.isDefined) for (meta <- metas.map(_._2)) {
    if (best.isDefined || !meta.prodId.isDefined) {
    } else if (meta.prodDatePrecision.ordinal >= Precision.MONTH.ordinal && meta.modDatePrecision.ordinal >= Precision.MONTH.ordinal &&
               meta.prodDate.substring(0,7) <= meta.modDate.substring(0,7)) {
      best = Some(meta)
    }
  }

  if (best.isDefined && metas.size > 1 && best.get.prodId.isDefined) for (meta <- metas.map(_._2)) {
    if (best.isDefined && meta.prodId.isDefined && meta.prodDate < best.get.prodDate &&
        meta.prodDatePrecision.ordinal >= best.get.prodDatePrecision.ordinal
    ) {
      best = Some(meta)
    }
  }

  if (!best.isDefined) {
    val prodDates = metas.map(_._2).filterNot(_.prodDate.isEmpty).map(_.prodDate)
    val partyDates = metas.map(_._2).filterNot(d => d.partyDate.isEmpty || d.partyDate.get.isEmpty()).map(_.partyDate.get)
    val minProdDate = if (prodDates.isEmpty) "" else prodDates.min
    val minPartyDate = if (partyDates.isEmpty) "" else partyDates.min
    best = metas.map(_._2).headOption.map(m =>
      val earliestDate = Seq(minProdDate, minPartyDate, m.modDate).filterNot(_.isEmpty).min
      m.copy(prodId = None, prod = "", prodPublishers = Seq.empty, prodDate = "", modDate = earliestDate))
  }
  
  best.map((md5, _))
}).toSeq
