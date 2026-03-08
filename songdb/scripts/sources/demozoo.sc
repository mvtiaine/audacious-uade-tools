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

import convert.MetaData
import oldexotica.metas
import sources.SourceDBEntry

enum Precision:
  case UNKNOWN, YEAR, MONTH, DATE

case class DemozooMeta (
  id: Int,
  title: String,
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
  prodType: Option[String]
)

// XXX
def fix(name: String) = name.replace(" - FIXME! This scener is a merge of two sceners!","")

def normalize(s: String) = s.toLowerCase.replaceAll("[^A-Za-z0-9]","").trim

def normalizePlatform(platform: String): String = {
  if (platform.startsWith("Amiga")) "Amiga"
  else if (platform.startsWith("Atari")) "Atari"
  else if (platform.startsWith("Windows")) "PC"
  else if (platform.startsWith("MS-Dos")) "PC"
  else ""
}

lazy val modland_by_path = sources.modland.groupBy(_.path.toLowerCase)
lazy val aminet_by_path = sources.aminet.groupBy(_.path
  .split("/").take(3).mkString("/").toLowerCase.replace(".lha","").replace(".lzx",""))
lazy val demozoo_leftovers_by_path = sources.demozoo_leftovers.groupBy(_.path.toLowerCase)
lazy val modarchive_by_id = sources.demozoo_leftovers
  .filter(_.path.startsWith("api.modarchive.org")).groupBy(_.path.split("/").take(2).last)
lazy val wantedteam_by_path = sources.wantedteam.groupBy(_.path.split("/").take(2).mkString("/").toLowerCase)
lazy val unexotica_by_path = sources.unexotica.groupBy(_.path.split("/").take(3).mkString("/").toLowerCase)
lazy val fujiology_by_path = sources.fujiology.groupBy(_.path.toLowerCase)
lazy val oldexotica_by_archive = oldexotica.metas.groupBy(_.archive.toLowerCase)
lazy val amigascne_by_path = sources.amigascne.groupBy(_.path.toLowerCase)
lazy val sceneorg_by_path = sources.sceneorg.groupBy(_.path.toLowerCase)
lazy val sceneorg_lostfound_by_path = sources.sceneorg_lostfound.groupBy(_.path.toLowerCase)
lazy val demodulate_by_path = sources.demodulate.groupBy(_.path.toLowerCase)
lazy val artpacksacidorg_by_path = sources.artpacksacidorg.groupBy(_.path.toLowerCase)
lazy val flerp_by_path = sources.flerp.groupBy(_.path.toLowerCase)
lazy val hornet_by_path = sources.hornet.groupBy(_.path.toLowerCase)
lazy val modsoulbrother_by_path = sources.modsoulbrother.groupBy(_.path.toLowerCase)
lazy val scenesporg_by_path = sources.scenesporg.groupBy(_.path.toLowerCase)
lazy val blastersoundbbs_by_path = sources.blastersoundbbs.groupBy(_.path.toLowerCase)
lazy val modplanet_by_path = sources.modplanet.groupBy(_.path.toLowerCase)

def trim(s: String) = {
  val trimmed = s.trim
    .replaceFirst("^\\{","").replaceAll("\\}$","")
    .replaceAll("\\\\","")
    .trim
  val res = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
    trimmed.replaceFirst("^\"","").replaceAll("\"$","").trim
  } else {
    trimmed
  }
  if (res == "NULL") "" else res
}
def split(s: String) = s
  .replaceFirst("\\{","")
  .replaceAll("\\}$","")
  .replace("Revelation Crew, The", "The Revelation Crew") // XXX
  .split(",")
  .filterNot(s => s == "NULL" || s.isEmpty)
def precision(s: String) = s match {
  case "y" => Precision.YEAR
  case "m" => Precision.MONTH
  case "d" => Precision.DATE
  case _ => Precision.UNKNOWN
}
def maybe(s: String) = {
  val trimmed = trim(s)
  if (trimmed == "NULL" || trimmed.isEmpty) None
  else Some(trimmed)
}

lazy val metas = Using(scala.io.Source.fromFile("sources/metadata/demozoo_music.tsv"))(_.getLines.toSeq.par.flatMap(line =>
  val l = line.split("\t")
  val id = l(0).toInt
  val title = l(1)
  val prodId = l(2).toIntOption
  val modDate = l(3)
  val modDatePrecision = precision(l(4))
  val prodDate = l(5)
  val prodDatePrecision = precision(l(6))
  val modPlatform = l(7)
  val prodPlatforms = split(l(8)) map trim
  val prod = l(9)
  val linkClass = l(10)
  val url = l(11).toLowerCase
  val authors = split(l(12)) map trim map fix
  val modPublishers = split(l(13)) map trim map fix
  val prodPublishers = split(l(14)) map trim map fix
  //val imageUrls = split(l(15))
  val party = if (l.length > 15) maybe(l(15)) else None
  val partyDate = if (l.length > 16) maybe(l(16)) else None
  val partyDatePrecision = if (l.length > 17) Some(precision(l(17))) else None
  val prodType = if (l.length > 18) maybe(l(18)) else None

  val meta = DemozooMeta(id, title, prodId, modDate, modDatePrecision, prodDate, prodDatePrecision,
    modPlatform, prodPlatforms.toSeq, prod, authors.toSeq, modPublishers.toSeq, prodPublishers.toSeq, // imageUrls.toSeq,
    party, partyDate, partyDatePrecision, prodType)

  def findMatches(meta: DemozooMeta, entries: Buffer[(String, String)]) = {
    val title = normalize(meta.title)
    val authors = meta.authors.map(normalize)
    val filenames = entries.map(e => (e._1, normalize(e._2.split("/").last)))
    var matches = filenames.filter(_._2.contains(title))
    if (matches.isEmpty) {
      matches = filenames.filter(f => authors.exists(a => f._2.contains(a)))
    } else if (matches.size > 1) {
      matches = matches.filter(f => authors.exists(a => f._2.contains(a)))
    }
    matches
  }

  def findArchive(archivePath: String, paths: Map[String, Seq[SourceDBEntry]] = demozoo_leftovers_by_path) = {
    val matches = paths.filterKeys(_.startsWith(archivePath))
    if (matches.isEmpty) {
      Buffer.empty[(String, String)]
    } else {
      var entries = matches.values.flatten
      if (entries.size > 1) {
        System.err.println("WARN: demozoo archive " + archivePath + " - multiple entries - " + entries.mkString(", "))
      }
      entries.map(e => (e.md5, e.path)).toBuffer.distinct
    }
  }

  def findLeftovers(path: String, paths: Map[String, Seq[SourceDBEntry]] = demozoo_leftovers_by_path) =
    if (paths.contains(path)) {
      val md5 = paths(path).head.md5
      Buffer((md5, meta))
    } else if (findArchive(path, paths).nonEmpty) {
      val md5s = findArchive(path, paths)
      if (md5s.size > 1) {
        val matches = findMatches(meta, md5s)
        if (matches.size == 1) {
          matches
            .map(m => (m._1, meta)) ++
          md5s.filterNot(m => matches.exists(_._1 == m._1))
            .map(md5 => (md5._1, meta.copy(authors = Seq.empty)))
        } else {
          md5s.map(md5 => (md5._1, meta.copy(authors = Seq.empty)))
        }
      } else {
        md5s.map(md5 => (md5._1, meta))
      }
    } else Buffer.empty

  // non-url links
  if (linkClass == "AmigascneFile") {
    val path = (if (url.startsWith("/")) url.drop(1) else url)
    findLeftovers(path, amigascne_by_path)
  } else if (linkClass == "ModarchiveModule") {
    if (modarchive_by_id.contains(url)) {
      val md5 = modarchive_by_id(url).head.md5
      Buffer((md5, meta))
    } else Buffer.empty
  } else if (linkClass == "ModlandFile" && url.startsWith("/pub/modules/")) {
    val path = url.replaceFirst("/pub/modules/", "").replace("//","/")
    if (modland_by_path.contains(path)) {
      val md5 = modland_by_path(path).head.md5
      Buffer((md5, meta))
    } else Buffer.empty
  } else if (linkClass == "PaduaOrgFile") {
    val path = "ftp.padua.org/pub/c64" + url
    findLeftovers(path)
  } else if (linkClass == "SceneOrgFile") {
    val path = (if (url.startsWith("/")) url.drop(1) else url)
    if (path.startsWith("demos/compilations/demodulate/")) {
      findLeftovers(path.replace("demos/compilations/demodulate/",""), demodulate_by_path)
    } else if (path.startsWith("demos/compilations/lost_found_and_more/")) {
      findLeftovers(path.replace("demos/compilations/lost_found_and_more/",""), sceneorg_lostfound_by_path)
    } else if (path.startsWith("mirrors/artpacks/")) {
      findLeftovers(path.replace("mirrors/artpacks/",""), artpacksacidorg_by_path)
    } else if (path.startsWith("mirrors/flerp/")) {
      findLeftovers(path.replace("mirrors/flerp/",""), flerp_by_path)
    } else if (path.startsWith("mirrors/hornet/")) {
      findLeftovers(path.replace("mirrors/hornet/",""), hornet_by_path)
    } else if (path.startsWith("mirrors/modsoulbrother/")) {
      findLeftovers(path.replace("mirrors/modsoulbrother/",""), modsoulbrother_by_path)
    } else if (path.startsWith("mirrors/scenesp.org/compilations/blastersound_bbs/")) {
      findLeftovers(path.replace("mirrors/scenesp.org/compilations/blastersound_bbs/",""), blastersoundbbs_by_path)
    } else if (path.startsWith("mirrors/scenesp.org/compilations/modplanet/normal/")) {
      findLeftovers(path.replace("mirrors/scenesp.org/compilations/modplanet/normal/",""), modplanet_by_path)
    } else if (path.startsWith("mirrors/scenesp.org/")) {
      findLeftovers(path.replace("mirrors/scenesp.org/",""), scenesporg_by_path)
    } else {
      findLeftovers(path, sceneorg_by_path)
    }
  } else if (linkClass == "UntergrundFile") {
    val path = "ftp.untergrund.net" + url
    findLeftovers(path)
  } else if (linkClass == "WaybackMachinePage") {
    val path = "web.archive.org/web/" + url
    findLeftovers(path)
  // embedded sources
  } else if (linkClass == "FujiologyFile") {
    val path = (if (url.startsWith("/")) url.drop(1) else url)
    findLeftovers(path, fujiology_by_path)
  } else if (url.contains("://amp.dascene.net/downmod.php?index=")) {
    val id = url.replaceAll("&application=amp","").split("=").last.toInt
    if (amp.amp_mods_by_id.contains(id)) {
      val md5 = amp.amp_mods_by_id(id).head.md5
      Buffer((md5, meta))
    } else Buffer.empty
  } else if (url.contains("://amp.dascene.net/modules/")) {
    // url should have been decoded already
    val path = url
      .replaceAll("http[s]?://amp.dascene.net/modules/","")
      .replace("//","/")
    if (amp.amp_by_path.contains(path)) {
      val md5 = amp.amp_by_path(path).head.md5
      Buffer((md5, meta))
    } else Buffer.empty
  } else if (url.contains("://aminet.net/")) {
    val path = url
      .replaceAll("http[s]?://aminet.net/package/","")
      .replaceAll("http[s]?://aminet.net/","")
      .replace("//","/")
      .replace(".lzx","")
      .replace(".lha","")
    if (aminet_by_path.contains(path)) {
      val entries = aminet_by_path(path)
      if (entries.size > 1) {
        System.err.println("WARN: aminet path " + path + " - multiple entries - " + entries.mkString(", "))
        val matches = findMatches(meta, entries.map(e => (e.md5, e.path)).toBuffer)
        if (matches.size == 1) {
          matches
            .map(m => (m._1, meta)) ++
          entries.filterNot(m => matches.exists(_._1 == m.md5))
            .map(e => (e.md5, meta.copy(authors = Seq.empty)))
        } else {
          entries.map(e => (e.md5, meta.copy(authors = Seq.empty)))
        }
      } else {
        entries.map(e => (e.md5, meta))
      }
    } else Buffer.empty
  } else if (url.contains("://wt.exotica.org.uk/files/")) {
    val path = url
      .replaceAll("http[s]?://wt.exotica.org.uk/files/","")
      .replace("//","/")
    if (wantedteam_by_path.contains(path)) {
      val md5 = wantedteam_by_path(path).head.md5
      Buffer((md5, meta))
    } else None
  } else if (url.contains("://files.exotica.org.uk/?file=exotica/media/audio/unexotica/") ||
             url.contains("://www.exotica.org.uk/download.php?file=media/audio/unexotica/") ||
             url.contains("://www.exotica.org.uk/tunes/archive/authors/")
  ) {
    val path = url
      .replaceAll("http[s]?://files.exotica.org.uk/\\?file=exotica/media/audio/unexotica/","")
      .replaceAll("http[s]?://www.exotica.org.uk/download.php\\?file=media/audio/unexotica/", "")
      .replaceAll("http[s]?://www.exotica.org.uk/tunes/archive/authors/", "")
      .replace("//","/")
    if (unexotica_by_path.contains(path)) {
      val entries = unexotica_by_path(path)
      if (entries.size > 1) {
        System.err.println("WARN: unexotica path " + path + " - multiple entries - " + entries)
        val matches = findMatches(meta, entries.map(e => (e.md5, e.path)).toBuffer)
        if (matches.size == 1) {
          matches
            .map(m => (m._1, meta)) ++
          entries.filterNot(m => matches.exists(_._1 == m.md5))
            .map(e => (e.md5, meta.copy(authors = Seq.empty)))
        } else {
          entries.map(e => (e.md5, meta.copy(authors = Seq.empty)))
        }
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
      if (oldexotica_by_archive.contains(archive)) {
        val entries = oldexotica_by_archive(archive)
        if (entries.size > 1) {
          System.err.println("WARN: oldexotica path " + archive + " - multiple entries - " + entries)
          val matches = findMatches(meta, entries.map(e => (e.md5, e.path)).toBuffer)
          if (matches.size == 1) {
            matches
              .map(m => (m._1, meta)) ++
            entries.filterNot(m => matches.exists(_._1 == m.md5))
              .map(e => (e.md5, meta.copy(authors = Seq.empty)))
          } else {
            entries.map(e => (e.md5, meta.copy(authors = Seq.empty)))
          }
        } else {
          entries.map(e => (e.md5, meta))
        }
      } else Buffer.empty
  // leftovers
  } else {
    val path = url
      .replaceAll("http[s]?://","")
      .replace("//","/")
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
      if (metas.size == 1 && m.partyDate.isDefined && m.partyDate.get.take(4).toInt == earliestDate.take(4).toInt) {
        m.copy(prodId = None, prod = "", prodPublishers = Seq.empty, prodDate = "", prodType = None, modDate = m.partyDate.get)
      } else if (metas.size == 1 && m.prodId.isDefined && earliestDate == minProdDate) {
        m.copy(modDate = earliestDate)
      } else {
        m.copy(prodId = None, prod = "", prodPublishers = Seq.empty, prodDate = "", prodType = None, modDate = earliestDate)
      }
    )
  }
  
  best.map((md5, _))
}).toSeq

lazy val demozooMetas = Using(scala.io.Source.fromFile("sources/metadata/demozoo_prods.tsv"))(_.getLines.toSeq.par.flatMap(line =>
  val l = line.split("\t")
  val prodId = l(0).toInt
  val prodDate = l(1)
  val prodDatePrecision = precision(l(2))
  var prod = l(3)
  val prodPlatforms = split(l(4)) map trim
  val prodPublishers = split(l(5)) map trim map fix
  val musicAuthors = split(l(6)) map trim map fix
  val party =  maybe(l(7))
  val partyDate = maybe(l(8))
  val partyDatePrecision = precision(l(9))
  val prodType = maybe(l(10))

  val authors = musicAuthors.sorted.distinct.toBuffer

  if (prodPlatforms.exists(p => p.startsWith("Amiga") || p.startsWith("MS-Dos") || p.startsWith("Windows") || p.startsWith("Atari Falcon") || p.startsWith("Atari Jaguar") || p.startsWith("Atari ST/E"))) {
    val meta = MetaData(
      hash = "",
      authors = if (authors.size > 2) Buffer.empty else authors,
      album = prod.trim,
      publishers = prodPublishers.sorted.distinct.toBuffer,
      year = prodDate.take(4).toIntOption.getOrElse(0),
      _type = prodType.getOrElse(""),
      _platform = if (prodPlatforms.isEmpty || prodPlatforms.size > 1) "" else normalizePlatform(prodPlatforms.head)
    )
    Some(meta)
  } else None
)).get.toSet
