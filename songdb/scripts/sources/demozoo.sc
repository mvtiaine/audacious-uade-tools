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

import convert._
import sources.Source
import sources.SourceDBEntry
import normalization._


enum Precision:
  case UNKNOWN, YEAR, MONTH, DATE

final case class DemozooMeta (
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
  partyShownDate: Option[String],
  partyShownDatePrecision: Option[Precision],
  partyStartDate: Option[String],
  partyStartDatePrecision: Option[Precision],
  prodType: Seq[String]
) {
  private val _partyDatePrecision: (Option[String], Option[Precision]) = {
    if (partyShownDate.isEmpty && partyStartDate.isEmpty) {
      (None, None)
    } else if (partyShownDate.nonEmpty && partyStartDate.nonEmpty) {
      val dates = Seq((partyShownDate.get, partyShownDatePrecision.get), (partyStartDate.get, partyStartDatePrecision.get))
      (Some(dates.minBy(_._1)._1), Some(dates.minBy(_._1)._2))
    } else if (partyShownDate.nonEmpty) {
      (partyShownDate, partyShownDatePrecision)
    } else {
      (partyStartDate, partyStartDatePrecision)
    }
  }
  val partyDate: Option[String] = _partyDatePrecision._1
  val partyDatePrecision: Option[Precision] = _partyDatePrecision._2
}

// XXX
def fix(name: String) = name.replace(" - FIXME! This scener is a merge of two sceners!","")

private def normalize(s: String) = s.toLowerCase.replaceAll("[^A-Za-z0-9]","").trim

private def normalizePlatform(platform: String): String = {
  if (platform.startsWith("Amiga")) "Amiga"
  else if (platform.startsWith("Atari")) "Atari"
  else if (platform.startsWith("Windows")) "PC"
  else if (platform.startsWith("Linux")) "PC"
  else if (platform.startsWith("MS-Dos")) "PC"
  else ""
}

val aminet_by_path = sources.sourceDB(Source.Aminet).groupBy(_.path
  .split("/").take(3).mkString("/").toLowerCase.replace(".lha","").replace(".lzx",""))
val demozoo_music_leftovers_by_path = sources.sourceDB(Source.DemozooMusicLeftovers).groupBy(_.path.toLowerCase)
val demozoo_prod_leftovers_by_path = sources.sourceDB(Source.DemozooProdLeftovers).groupBy(_.path.toLowerCase)
val modarchive_by_id = sources.sourceDB(Source.DemozooMusicLeftovers)
  .filter(_.path.startsWith("api.modarchive.org")).groupBy(_.path.split("/").take(2).last)
val fujiology_by_path = sources.sourceDB(Source.Fujiology).groupBy(_.path.toLowerCase)
val sceneorg_by_path = sources.sourceDB(Source.SceneOrg).groupBy(_.path.toLowerCase)
val sceneorg_lostfound_by_path = sources.sourceDB(Source.SceneOrgLostFound).groupBy(_.path.toLowerCase)
val demodulate_by_path = sources.sourceDB(Source.Demodulate).groupBy(_.path.toLowerCase)
val artpacksacidorg_by_path = sources.sourceDB(Source.ArtPacksAcidOrg).groupBy(_.path.toLowerCase)
val flerp_by_path = sources.sourceDB(Source.Flerp).groupBy(_.path.toLowerCase)
val hornet_by_path = sources.sourceDB(Source.Hornet).groupBy(_.path.toLowerCase)
val modsoulbrother_by_path = sources.sourceDB(Source.ModSoulBrother).groupBy(_.path.toLowerCase)
val scenesporg_by_path = sources.sourceDB(Source.SceneSporg).groupBy(_.path.toLowerCase)
val blastersoundbbs_by_path = sources.sourceDB(Source.BlasterSoundBBS).groupBy(_.path.toLowerCase)
val modplanet_by_path = sources.sourceDB(Source.ModPlanet).groupBy(_.path.toLowerCase)
val jpvscenereleases_by_path = sources.sourceDB(Source.jPVSceneReleases).groupBy(_.path.toLowerCase)

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
  .toBuffer
def precision(s: String) = s match {
  case "y" => Precision.YEAR
  case "m" => Precision.MONTH
  case "d" => Precision.DATE
  case _ => Precision.UNKNOWN
}
def date(date: String, precision: Precision, emptyMissing: Boolean = true) = precision match {
  case Precision.YEAR => date.substring(0, 4) + "-99-99"
  case Precision.MONTH => date.substring(0, 7) + "-99"
  case Precision.DATE => date
  case _ => if (emptyMissing) "" else "9999-99-99"
}
def maybe(s: String) = {
  val trimmed = trim(s)
  if (trimmed == "NULL" || trimmed.isEmpty) None
  else Some(trimmed)
}

lazy val metas = Using(scala.io.Source.fromFile("sources/metadata/demozoo_music.tsv"))(_.getLines().toSeq.par.flatMap(line =>
  val l = line.split("\t")
  val id = l(0).toInt
  val title = l(1)
  val prodId = l(2).toIntOption
  val modDatePrecision = precision(l(4))
  val modDate = date(l(3), modDatePrecision)
  val prodDatePrecision = precision(l(6))
  val prodDate = date(l(5), prodDatePrecision)
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
  val partyShownDatePrecision = if (l.length > 17) maybe(l(17)).map(precision) else None
  val partyShownDate = (if (l.length > 16) maybe(l(16)) else None).map(d => date(d, partyShownDatePrecision.getOrElse(Precision.UNKNOWN)))
  val partyStartDatePrecision = (if (l.length > 19) maybe(l(19)).map(precision) else None)
  val partyStartDate = (if (l.length > 18) maybe(l(18)) else None).map(d => date(d, partyStartDatePrecision.getOrElse(Precision.UNKNOWN)))
  val prodType = if (l.length > 20) split(l(20)).map(trim).toSeq else Seq.empty

  val meta = DemozooMeta(id, title, prodId, modDate, modDatePrecision, prodDate, prodDatePrecision,
    modPlatform, prodPlatforms.toSeq, prod, authors.toSeq, modPublishers.toSeq, prodPublishers.toSeq, // imageUrls.toSeq,
    party, partyShownDate, partyShownDatePrecision, partyStartDate, partyStartDatePrecision, prodType)

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

  def findArchive(archivePath: String, paths: Map[String, Seq[SourceDBEntry]] = demozoo_music_leftovers_by_path) =
    sources.findArchive(archivePath, paths)

  def findLeftovers(path: String, paths: Map[String, Seq[SourceDBEntry]] = demozoo_music_leftovers_by_path) = {
    if (paths.contains(path)) {
      val md5 = paths(path).head.md5
      Buffer((md5, meta))
    } else {
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
    }
  }

  // non-url links
  if (linkClass == "AmigascneFile") {
    val path = (if (url.startsWith("/")) url.drop(1) else url)
    findLeftovers(path, sources.amigascne_by_path)
  } else if (linkClass == "ModarchiveModule") {
    if (modarchive_by_id.contains(url)) {
      val md5 = modarchive_by_id(url).head.md5
      Buffer((md5, meta))
    } else Buffer.empty
  } else if (linkClass == "ModlandFile" && url.startsWith("/pub/modules/")) {
    val path = url.replaceFirst("/pub/modules//?", "").replace("//","/")
    if (sources.modland_by_path.contains(path)) {
      val md5 = sources.modland_by_path(path).head.md5
      Buffer((md5, meta))
    // XXX /pub/modules/Fabian/dlm4-shortcutted.mod  etc.
    } else if (path.endsWith(".mod") && !path.startsWith("protracker/")) {
      val altPath = "protracker/" + path
      if (sources.modland_by_path.contains(altPath)) {
        val md5 = sources.modland_by_path(altPath).head.md5
        Buffer((md5, meta))
      } else Buffer.empty
    // XXX /pub/modules/Protracker/Karsten%20Obarski/sleepwalk.mod etc.
    } else if (path.endsWith(".mod") && path.startsWith("protracker/")) {
      val altPath = path.replaceFirst("protracker/", "soundtracker/")
      if (sources.modland_by_path.contains(altPath)) {
        val md5 = sources.modland_by_path(altPath).head.md5
        Buffer((md5, meta))
      } else Buffer.empty
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
  } else if (url.contains("://amp.dascene.net/downmod.php?index=") ||
             url.contains("://amp.dascene.net/analyzer2.php?idx=")) {
    val id = url.replaceAll("&application=amp","").split("=").last.toInt
    if (amp.amp_mods_by_id.contains(id)) {
      val md5 = amp.amp_mods_by_id(id).head.md5
      Buffer((md5, meta))
    } else Buffer.empty
  } else if (url.contains("://amp.dascene.net/modules/")) {
    // url should have been decoded already
    val path = url
      .replaceAll("http[s]?://amp.dascene.net/modules//?","")
      .replace("//","/")
    if (sources.amp_by_path.contains(path)) {
      val md5 = sources.amp_by_path(path).head.md5
      Buffer((md5, meta))
    } else Buffer.empty
  } else if (url.contains("://aminet.net/") || url.contains(".aminet.net/")) {
    val path = url
      .replaceAll("http[s]?://.*aminet.net/package//?","")
      .replaceAll("http[s]?:/.*aminet.net//?","")
      .replaceAll("ftp://.*aminet.net//?","")
      .replace("pub/aminet/", "")
      .replace("aminet/", "")
      .replace("//","/")
      .replace(".lzx","")
      .replace(".lha","")
    if (aminet_by_path.contains(path)) {
      val entries = aminet_by_path(path)
      if (entries.size > 1) {
        //System.err.println("WARN: aminet path " + path + " - multiple entries - " + entries.mkString(", "))
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
      .replaceAll("http[s]?://wt.exotica.org.uk/files//?","")
      .replace("//","/")
    if (sources.wantedteam_by_path.contains(path)) {
      val md5 = sources.wantedteam_by_path(path).head.md5
      Buffer((md5, meta))
    } else None
  } else if (url.contains("://files.exotica.org.uk/?file=exotica/media/audio/unexotica/") ||
             url.contains("://www.exotica.org.uk/download.php?file=media/audio/unexotica/") ||
             url.contains("://www.exotica.org.uk/tunes/archive/authors/")
  ) {
    val path = url
      .replaceAll("http[s]?://files.exotica.org.uk/\\?file=exotica/media/audio/unexotica//?","")
      .replaceAll("http[s]?://www.exotica.org.uk/download.php\\?file=media/audio/unexotica//?","")
      .replaceAll("http[s]?://www.exotica.org.uk/tunes/archive/authors//?","")
      .replace("//","/")
    if (sources.unexotica_by_path.contains(path)) {
      val entries = sources.unexotica_by_path(path)
      if (entries.size > 1) {
        //System.err.println("WARN: unexotica path " + path + " - multiple entries - " + entries)
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
        .replaceAll("http[s]?://www.exotica.org.uk/tunes/archive//?","")
        .replaceAll("http[s]?://old.exotica.org.uk/tunes/archive//?","")
        .replace("//","/")
      if (oldexotica.oldexotica_by_archive.contains(archive)) {
        val entries = oldexotica.oldexotica_by_archive(archive)
        if (entries.size > 1) {
          //System.err.println("WARN: oldexotica path " + archive + " - multiple entries - " + entries)
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
      .replaceAll("ftp://","")
      .replace("//","/")
    findLeftovers(path)
  }

).distinct.toBuffer).get.groupBy(_._1).flatMap({case (md5, metas_) =>
  var metas = {
    if (metas_.exists(_._2.authors.nonEmpty)) metas_.filter(_._2.authors.nonEmpty)
    else metas_
  }.map(_._2).distinct
  metas = {
    if (metas.exists(m => m.prodDate.nonEmpty || m.partyDate.nonEmpty)) metas.filter(m => m.prodDate.nonEmpty || m.partyDate.nonEmpty)
    else metas
  }
  if (metas.forall(m => m.modDate == metas.head.modDate)) {
    metas = metas.sortBy(m => Seq((if (m.prodDate.nonEmpty) m.prodDate else "9999-99-99"), Seq(m.partyShownDate.getOrElse(""), m.partyStartDate.getOrElse("")).filter(_.nonEmpty).maxOption.getOrElse("9999-99-99")).min)
  } else {
    metas = metas.sortBy(m => Seq(Seq(m.partyShownDate.getOrElse(""), m.partyStartDate.getOrElse("")).filter(_.nonEmpty).maxOption.getOrElse("9999-99-99"), (if (m.prodDate.nonEmpty) m.prodDate else "9999-99-99"), if (m.modDate.nonEmpty) m.modDate else "9999-99-99").min)
  }
  var best: Option[DemozooMeta] = None
  if (metas.size == 1) {
    best = Some(metas.head)
  } else {
    val prodDates = metas.filterNot(_.prodDate.isEmpty).map(_.prodDate)
    val partyDates = metas.filterNot(d => d.partyDate.isEmpty || d.partyDate.get.isEmpty()).map(_.partyDate.get)
    val modDates = metas.filterNot(_.modDate.isEmpty).map(_.modDate)
    val minProdDate = if (prodDates.isEmpty) "9999-99-99" else prodDates.min
    val minPartyDate = if (partyDates.isEmpty) "9999-99-99" else partyDates.min
    val minModDate = if (modDates.isEmpty) "9999-99-99" else modDates.min
    val earliestDate = {
      val dates = Seq(minProdDate, minPartyDate, minModDate).filterNot(_.isEmpty)
      if (dates.isEmpty) "9999-99-99" else dates.min
    }
    for (meta <- metas) {
      val digits =
        if (meta.modDatePrecision == Precision.YEAR) 4
        else if (meta.modDatePrecision == Precision.MONTH) 7
        else if (meta.partyDate.isDefined && meta.partyDatePrecision.get == Precision.DATE || (best.isDefined && best.get.partyDate.isDefined && best.get.partyDatePrecision.get == Precision.DATE)) 10 else 7
      val prodCandidate0 = meta.prodDate.nonEmpty && meta.prodDate == meta.modDate && meta.modDatePrecision == meta.prodDatePrecision &&
        meta.prodDate.take(digits) <= earliestDate.take(digits)
      val partyCandidate0 = meta.partyDate.nonEmpty && meta.partyDate.getOrElse("9999-99-99") == meta.modDate && meta.modDatePrecision == meta.partyDatePrecision.getOrElse(Precision.UNKNOWN) &&
        meta.partyDate.getOrElse("9999-99-99").take(digits) <= earliestDate.take(digits)
      val prodCandidate1 = meta.prodDate.nonEmpty && meta.prodDate <= earliestDate && meta.modDatePrecision == meta.prodDatePrecision
      val partyCandidate1 = meta.partyDate.nonEmpty && meta.partyDate.getOrElse("9999-99-99") <= earliestDate && meta.modDatePrecision == meta.partyDatePrecision.getOrElse(Precision.UNKNOWN)
      val prodCandidate2 = meta.prodDate.nonEmpty && meta.prodDate <= earliestDate
      val partyCandidate2 = meta.partyDate.nonEmpty && meta.partyDate.getOrElse("9999-99-99") <= earliestDate

      val bestProdCandidate0 = best.exists(b => b.prodDate.nonEmpty && b.prodDate == b.modDate && b.modDatePrecision == b.prodDatePrecision && b.prodDate.take(digits) <= earliestDate.take(digits))
      val bestPartyCandidate0 = best.exists(b => b.partyDate.nonEmpty && b.partyDate.getOrElse("9999-99-99") == b.modDate && b.modDatePrecision == b.partyDatePrecision.getOrElse(Precision.UNKNOWN) && b.partyDate.getOrElse("9999-99-99").take(digits) <= earliestDate.take(digits))
      val bestProdCandidate1 = best.exists(b => b.prodDate.nonEmpty && b.prodDate <= earliestDate && b.modDatePrecision == b.prodDatePrecision)
      val bestPartyCandidate1 = best.exists(b => b.partyDate.nonEmpty && b.partyDate.getOrElse("9999-99-99") <= earliestDate && b.modDatePrecision == b.partyDatePrecision.getOrElse(Precision.UNKNOWN))
      val bestProdCandidate2 = best.exists(b => b.prodDate.nonEmpty && b.prodDate.nonEmpty && b.prodDate <= earliestDate)
      val bestPartyCandidate2 = best.exists(b => b.partyDate.nonEmpty && b.partyDate.getOrElse("9999-99-99") <= earliestDate)

      if (!best.isDefined) {
        if (prodCandidate0 || partyCandidate0 || prodCandidate1 || partyCandidate1 || prodCandidate2 || partyCandidate2) { 
          best = Some(meta)
        }
      } else {
        if (prodCandidate0 && !bestProdCandidate0 && !bestPartyCandidate0 && !bestProdCandidate1 && !bestPartyCandidate1) {
          best = Some(meta)
        } else if (partyCandidate1 && !bestProdCandidate0 && !bestPartyCandidate0 && !bestProdCandidate1 && !bestPartyCandidate1) {
          best = Some(meta)
        } else if (prodCandidate1 && !bestProdCandidate0 && !bestPartyCandidate0 && !bestProdCandidate1 && !bestPartyCandidate1) {
          best = Some(meta)
        } else if (partyCandidate1 && !bestProdCandidate0 && !bestPartyCandidate0 && !bestProdCandidate1 && !bestPartyCandidate1) {
          best = Some(meta)
        } else if (prodCandidate2 && !bestProdCandidate0 && !bestPartyCandidate0 && !bestProdCandidate1 && !bestPartyCandidate1 && !bestProdCandidate2 && !bestPartyCandidate2) {
          best = Some(meta)
        } else if (partyCandidate2 && !bestProdCandidate0 && !bestPartyCandidate0 && !bestProdCandidate1 && !bestPartyCandidate1 && !bestProdCandidate2 && !bestPartyCandidate2) {
          best = Some(meta)
        }
      }
    }
  }

  if (!best.isDefined) {
    metas = metas.sortBy(m => Seq((if (m.prodDate.nonEmpty) m.prodDate else "9999-99-99"), Seq(m.partyShownDate.getOrElse(""), m.partyStartDate.getOrElse("")).filter(_.nonEmpty).maxOption.getOrElse("9999-99-99")).min)
    for (meta <- metas) {
      if (best.isDefined || (meta.prodId.isEmpty && meta.partyDate.isEmpty)) {
      } else if (!best.isDefined) {
        best = Some(meta)
      }
    }
  }

  if (!best.isDefined) {
    metas = metas.sortBy(_.modDate)
    best = Some(metas.head)
  }

  val bestYear = if (best.get.prodDate.nonEmpty) best.get.prodDate.take(4).toInt else 0
  val bestMonth = if (best.get.prodDate.nonEmpty) best.get.prodDate.drop(5).take(2).toInt else 0
  var maxMonthDiff = Int.MaxValue
  var prodCount = if (best.get.prodId.nonEmpty) 1 else 0
  metas.foreach(m => best.foreach(_ => {
    if (m != best.get && m.prodDate.nonEmpty && best.get.prodDate.nonEmpty) {
      val prodYear = m.prodDate.take(4).toInt
      var prodMonth = m.prodDate.drop(5).take(2).toInt
      if (m.prodDatePrecision.ordinal <= Precision.YEAR.ordinal || best.get.prodDatePrecision.ordinal <= Precision.YEAR.ordinal) {
        prodMonth = if (prodYear == bestYear) bestMonth else if (prodYear > bestYear) bestMonth + 1 else bestMonth - 1
      }
      maxMonthDiff = math.min(maxMonthDiff, math.abs((prodYear - bestYear) * 12 + Math.min(prodMonth - bestMonth, 11)))
    }
    if (m != best.get && m.prodId.nonEmpty)
      prodCount += 1
  }))

  best.map(b => b.copy(modDate = b.modDate.replace("-99", "-01"), prodDate = b.prodDate.replace("-99", "-01"), partyShownDate = b.partyShownDate.map(_ => b.partyShownDate.get.replace("-99", "-01")), partyStartDate = b.partyStartDate.map(_ => b.partyStartDate.get.replace("-99", "-01"))))
  .map((md5, _, prodCount, maxMonthDiff))
}).toSeq

final case class DemozooSoundtrack (
  id: Int,
  title: String,
  modDate: String,
  modDatePrecision: Precision,
  authors: Buffer[String],
)

val soundtracksByProdId = Using(scala.io.Source.fromFile("sources/metadata/demozoo_soundtracks.tsv"))(_.getLines().toSeq.par.map(line =>
  val l = line.split("\t")
  val id = l(0).toInt
  val title = l(1)
  val modDatePrecision = precision(l(3))
  val modDate = date(l(2), modDatePrecision)
  val authors = (split(l(4)) map trim map fix).sorted.distinct.toBuffer
  val prodIds = split(l(5)).flatMap(_.toIntOption)

  prodIds.map(prodId => (prodId, DemozooSoundtrack(id, title, modDate, modDatePrecision, authors)))
)).get.flatten.groupBy(_._1).par.mapValues(_.map(_._2).distinct.seq).seq

final case class DemozooProdMeta (
  id: Int,
  date: String,
  datePrecision: Precision,
  title: String,
  platforms: Buffer[String],
  publishers: Buffer[String],
  musicAuthors: Buffer[String],
  party: Option[String],
  partyShownDate: Option[String],
  partyShownDatePrecision: Option[Precision],
  partyStartDate: Option[String],
  partyStartDatePrecision: Option[Precision],
  prodType: Seq[String],
  soundtrackIds: Seq[Int],
  linkClass: String,
  url: String
) {
  private val _partyDatePrecision: (Option[String], Option[Precision]) = {
    if (partyShownDate.isEmpty && partyStartDate.isEmpty) {
      (None, None)
    } else if (partyShownDate.nonEmpty && partyStartDate.nonEmpty) {
      val dates = Seq((partyShownDate.get, partyShownDatePrecision.get), (partyStartDate.get, partyStartDatePrecision.get))
      (Some(dates.minBy(_._1)._1), Some(dates.minBy(_._1)._2))
    } else if (partyShownDate.nonEmpty) {
      (partyShownDate, partyShownDatePrecision)
    } else {
      (partyStartDate, partyStartDatePrecision)
    }
  }
  val partyDate: Option[String] = _partyDatePrecision._1
  val partyDatePrecision: Option[Precision] = _partyDatePrecision._2
}

private lazy val demozooProdMetas = Using(scala.io.Source.fromFile("sources/metadata/demozoo_prods.tsv"))(_.getLines().toSeq.par.flatMap(line =>
  val l = line.split("\t")
  val id = l(0).toInt
  val date = l(1)
  val datePrecision = precision(l(2))
  val title = l(3)
  val platforms = split(l(4)) map trim
  val publishers = split(l(5)) map trim map fix
  val musicAuthors = (split(l(6)) map trim map fix).sorted.distinct.toBuffer
  val party = maybe(l(7))
  val partyShownDate = maybe(l(8))
  val partyShownDatePrecision = maybe(l(9)).map(precision)
  val partyStartDate = maybe(l(10))
  val partyStartDatePrecision = maybe(l(11)).map(precision)
  val prodType = split(l(12)).map(trim)
  val soundtrackIds = split(l(13)).flatMap(_.toIntOption)
  val linkClass = l(14)
  val url = l(15).toLowerCase

  if (platforms.exists(p => p.startsWith("Amiga") || p.startsWith("MS-Dos") || p.startsWith("Windows") || p.startsWith("Atari Falcon") || p.startsWith("Atari Jaguar") || p.startsWith("Atari ST/E")) || (platforms.isEmpty && !url.contains("/demos/compilations/lost_found_and_more/chip_module_collection_no1"))) {
    Some(DemozooProdMeta(id, date, datePrecision, title, platforms.toBuffer, publishers.toBuffer, musicAuthors, party, partyShownDate, partyShownDatePrecision, partyStartDate, partyStartDatePrecision, prodType.toSeq, soundtrackIds.toSeq, linkClass, url))
  } else {
    None
  }
)).get.seq.groupBy(_.id)

lazy val demozooMetas = demozooProdMetas.values.par.map(_.map(prod => {

  val soundtracks = soundtracksByProdId.getOrElse(prod.id, Seq.empty)
  val soundtrackCount = soundtracks.map(_.id).distinct.size
  var authors = Seq.empty[Buffer[String]]

  if (soundtrackCount > 0) {
    authors = soundtracks.map(_.authors).filter(_.nonEmpty).distinct
  }
  if (prod.musicAuthors.size <= 2 && soundtrackCount == 0) {
    authors = Seq(prod.musicAuthors)
  } else if (prod.musicAuthors.exists(a => a.nonEmpty && !soundtracks.exists(_.authors.contains(a)))) {
    prod.musicAuthors.filter(a => a.nonEmpty && !soundtracks.exists(_.authors.contains(a))).foreach(a => {
      authors = (authors :+ Buffer(a)).distinct
    })
  }
    
  val metas = if (authors.nonEmpty) {
    authors.distinct.map(a => {
      Buffer((prod.id, MetaData(
        hash = "",
        authors = a.sorted.distinct,
        album = prod.title.trim,
        publishers = prod.publishers.sorted.distinct.toBuffer,
        year = prod.date.take(4).toIntOption.getOrElse(0)
      )))
    }).flatten
  } else {
    Buffer((prod.id, MetaData(
      hash = "",
      authors = Buffer.empty,
      album = prod.title.trim,
      publishers = prod.publishers.sorted.distinct.toBuffer,
      year = prod.date.take(4).toIntOption.getOrElse(0)
    )))
  }.toBuffer
  var _types = if (prod.prodType.nonEmpty) prod.prodType.filter(_.nonEmpty).sorted.distinct else Buffer("")
  var normPlatforms = (prod.platforms.map(normalizePlatform).filter(_.nonEmpty).sorted.distinct.nonEmpty match {
    case true => prod.platforms.map(normalizePlatform).filter(_.nonEmpty).sorted.distinct
    case false => Buffer("")
  })
  metas.map(meta => {
    var metas = Buffer.empty[(Int, MetaData)]
    for (t <- _types) {
      for (p <- normPlatforms) {
        metas = metas :+ (meta._1, meta._2.copy(_type = t, _platform = p))
      }
    }
    metas
  }).flatten
}).flatten).flatten.seq.toSet

for ((id, meta) <- demozooMetas) {
  println(s"DEMOZOO PROD META: ${id}: ${meta}")
}

lazy val all_aliases = Using(scala.io.Source.fromFile("sources/metadata/demozoo_authors.tsv"))(_.getLines().toSeq.par.flatMap(line =>
    // For each handle, collect all normalized aliases, then map each alias to the full set
  val l = line.split("\t")
  val releaser_name = l(1).trim
  val nicks = split(l(2)).map(trim).filter(_.nonEmpty)
  val nick_variants = split(l(3)).map(trim).filter(_.nonEmpty)

  val names = (Seq(releaser_name) ++ nicks ++ nick_variants).distinct
  if (names.nonEmpty) {
    val normalized = names.map(normalizeAuthor)
    normalized.map(n => n -> names.distinct.toBuffer)
  } else Iterable.empty[(String, Buffer[String])]
)).get.seq.groupBy(_._1).view.mapValues(_.flatMap(_._2).toBuffer.distinct).toMap

def transformMeta(md5: String, m: DemozooMeta, prodCount: Int, maxMonthDiff: Int): Option[MetaData] = {
  val dates = Seq(m.modDate, m.prodDate, Seq(m.partyShownDate.getOrElse(""), m.partyStartDate.getOrElse("")).max).filterNot(_.isEmpty)
  val digits =
    if (m.modDatePrecision == Precision.YEAR) 4
    else if (m.modDatePrecision == Precision.MONTH) 7
    else if (m.partyDate.isDefined && m.partyDatePrecision.get == Precision.DATE && m.partyDate.get <= m.modDate) 10
    else 7
  val earliestDate = if (dates.isEmpty) "" else dates.min
  val cmpDate = if (earliestDate.isEmpty) "9999-99-99" else earliestDate
  val authors = m.authors.filterNot(_ == "?").sorted.toBuffer
  var useProd = !m.prod.isEmpty && (m.prodDate.take(digits) <= cmpDate.take(digits))
  var useParty = m.party.isDefined && m.partyDate.getOrElse("9999-99-99").take(4) <= cmpDate.take(4)

  if (useProd && useParty) {
    val prodDate = if (m.prodDatePrecision == Precision.YEAR) m.prodDate.replace("-01-01", "-99-99") else if (m.prodDatePrecision == Precision.MONTH) m.prodDate.replaceAll("-01$", "-99") else m.prodDate
    val partyShownDate =
      if (m.partyShownDate.isEmpty) ""
      else if (m.partyShownDatePrecision.get == Precision.YEAR) m.partyShownDate.get.replace("-01-01", "-99-99") else if (m.partyShownDatePrecision.get == Precision.MONTH) m.partyShownDate.get.replaceAll("-01$", "-99") else m.partyShownDate.get
    val partyStartDate =
      if (m.partyStartDate.isEmpty) ""
      else if (m.partyStartDatePrecision.get == Precision.YEAR) m.partyStartDate.get.replace("-01-01", "-99-99") else if (m.partyStartDatePrecision.get == Precision.MONTH) m.partyStartDate.get.replaceAll("-01$", "-99") else m.partyStartDate.get
    val digits = Precision.fromOrdinal(Seq(m.partyShownDatePrecision.getOrElse(m.partyStartDatePrecision.get).ordinal, m.partyStartDatePrecision.getOrElse(m.partyShownDatePrecision.get).ordinal).max) match {
      case Precision.YEAR => 4
      case Precision.MONTH => 7
      case _ => 10
    }
    useProd = prodDate.take(digits) <= (if (partyShownDate.nonEmpty) partyShownDate.take(digits) else partyStartDate.take(digits)) ||
              prodDate.take(digits) <= (if (partyStartDate.nonEmpty) partyStartDate.take(digits) else partyShownDate.take(digits))
    useParty = !useProd
  }
  if (!useParty && !useProd && m.party.isDefined && m.partyDate.isDefined && m.partyDate.get.take(4).toInt <= cmpDate.take(4).toInt + 1 && (m.prodDate.isEmpty || m.prodDate.take(4).toInt >= m.partyDate.get.take(4).toInt)) {
    useParty = true
  }
  if (!useParty && !useProd && m.party.isDefined && m.partyDate.isDefined && m.prodDate.nonEmpty && m.prodDate.take(4).toInt <= cmpDate.take(4).toInt + 1) {
    useProd = true
  }
  val monthDiff = if (m.prodDate.nonEmpty) {
    val prodYear = m.prodDate.take(4).toInt
    val prodMonth = m.prodDate.drop(5).take(2).toInt
    val cmpYear = cmpDate.take(4).toInt
    val cmpMonth = cmpDate.drop(5).take(2).toInt
    Math.abs(prodYear - cmpYear) * 12 + Math.min(prodMonth - cmpMonth, 11)
  } else 0
  if (!useParty && !useProd && !m.party.isDefined && !m.partyDate.isDefined && m.prodDate.nonEmpty && monthDiff <= maxMonthDiff && monthDiff <= 12) {
    useProd = true
  }
  if (!useParty && !useProd && m.prodId.nonEmpty && prodCount == 1 && monthDiff <= 36 &&
      !m.modPublishers.isEmpty && !m.prodPublishers.isEmpty && (
      (m.modPublishers.exists(mp => m.prodPublishers.exists(pp => mp.equalsIgnoreCase(pp)))) ||
      (m.prodPublishers.exists(pp => m.modPublishers.exists(mp => pp.equalsIgnoreCase(mp)))))
    ) {
    useProd = true
  }
  if (!useParty && !useProd && m.prodId.nonEmpty && prodCount == 1 && monthDiff <= 12) {
    useProd = true
  }
  val publishDate =
    if (useProd) {
      if (m.prodDate.nonEmpty) m.prodDate else earliestDate
    } else if (useParty) {
      m.partyDate.getOrElse(earliestDate)
    } else {
      earliestDate
    }
  val normPlatforms = m.prodPlatforms.map(normalizePlatform).sorted.distinct
  val info = MetaData(
    hash = md5.take(12),
    authors = if (authors.forall(_.trim.isEmpty)) Buffer.empty else authors,
    publishers = ((m.prodPublishers, m.party, m.modPublishers) match {
      case (prod,_,_) if useProd =>
        if (prod.forall(_.trim.isEmpty)) Buffer.empty else prod.toBuffer
      case (_,party,_) if useParty =>
        Buffer(party.get)
      case (_,_,mod) if !mod.isEmpty =>
        if (mod.forall(_.trim.isEmpty)) Buffer.empty else mod.toBuffer
      case _ => Buffer.empty
    }).sorted,
    album = if (useProd) m.prod.trim else "",
    year = if (!publishDate.isEmpty) publishDate.substring(0,4).toInt else 0,
    _type = if (useProd) m.prodType.sorted.headOption.getOrElse("") else if (useParty) "Compo" else "",
    _platform = if (!useProd || normPlatforms.isEmpty || normPlatforms.size > 1) "" else normPlatforms.head
  )
  info match {
    case MetaData(_, Buffer(), Buffer(), "", 0, "", "") => None
    case _ => Some(info)
  }
}.map(m =>
  // XXX 5th Gear (Amiga)
  if (m.hash == "4a17ff0d3fb2" || m.hash == "a9199ea17a17") m.copy(year = 1990)
  else m
)
val typeBlacklist = Set(
  "Pack",
  "Tool",
)
val demozooExtras = demozooProdMetas.filterNot { case (prodId, metas) =>
  metas.exists(_.prodType.forall(typeBlacklist.contains)) ||
  // XXX
  metas.exists(_.title == "Fading Twilight - Dual Layer DVD Edition") ||
  metas.exists(_.title == "Tracker Hero") ||
  prodId == 303858
}.par.map { case (prodId, metas) =>
  metas.flatMap { meta =>
    var md5s = Buffer.empty[String]
    val path = (if (meta.url.startsWith("/")) meta.url.drop(1) else meta.url).toLowerCase
    if (meta.linkClass == "AmigascneFile") {
      md5s = sources.findArchive(path, sources.amigascne_by_path).map(_._1).sorted.distinct
    } else if (meta.linkClass == "ModlandFile" && meta.url.startsWith("/pub/modules/")) {
      val path = meta.url.replaceFirst("/pub/modules//?", "").replace("//","/").toLowerCase
      if (sources.modland_by_path.contains(path)) {
        val md5 = sources.modland_by_path(path).head.md5
        md5s = Buffer(md5)
      }
    } else if (meta.linkClass == "SceneOrgFile") {
      if (path.startsWith("demos/compilations/lost_found_and_more/")) {
        md5s = sources.findArchive(path.replace("demos/compilations/lost_found_and_more/",""), sceneorg_lostfound_by_path).map(_._1).sorted.distinct
      } else if (path.startsWith("mirrors/artpacks/")) {
        md5s = sources.findArchive(path.replace("mirrors/artpacks/",""), artpacksacidorg_by_path).map(_._1).sorted.distinct
      } else if (path.startsWith("mirrors/flerp/")) {
        md5s = sources.findArchive(path.replace("mirrors/flerp/",""), flerp_by_path).map(_._1).sorted.distinct
      } else if (path.startsWith("mirrors/hornet/")) {
        md5s = sources.findArchive(path.replace("mirrors/hornet/",""), hornet_by_path).map(_._1).sorted.distinct
      } else if (path.startsWith("mirrors/scenesp.org/compilations/blastersound_bbs/")) {
        md5s = sources.findArchive(path.replace("mirrors/scenesp.org/compilations/blastersound_bbs/",""), blastersoundbbs_by_path).map(_._1).sorted.distinct
      } else if (path.startsWith("mirrors/scenesp.org/compilations/modplanet/normal/")) {
        md5s = sources.findArchive(path.replace("mirrors/scenesp.org/compilations/modplanet/normal/",""), modplanet_by_path).map(_._1).sorted.distinct
      } else if (path.startsWith("mirrors/scenesp.org/")) {
        md5s = sources.findArchive(path.replace("mirrors/scenesp.org/",""), scenesporg_by_path).map(_._1).sorted.distinct
      } else {
        md5s = sources.findArchive(path, sceneorg_by_path).map(_._1).sorted.distinct
      }
    } else if (meta.linkClass == "UntergrundFile") {
      val path = "ftp.untergrund.net" + meta.url.toLowerCase
      md5s = sources.findArchive(path, demozoo_prod_leftovers_by_path).map(_._1).sorted.distinct
    } else if (meta.linkClass == "WaybackMachinePage") {
      val path = "web.archive.org/web/" + meta.url.toLowerCase
      md5s = sources.findArchive(path, demozoo_prod_leftovers_by_path).map(_._1).sorted.distinct
    } else if (meta.linkClass == "FujiologyFile") {
      md5s = sources.findArchive(path, fujiology_by_path).map(_._1).sorted.distinct
    } else if (meta.url.contains("://amp.dascene.net/downmod.php?index=") ||
               meta.url.contains("://amp.dascene.net/analyzer2.php?idx=")) {
      val id = meta.url.toLowerCase.replaceAll("&application=amp","").split("=").last.toInt
      if (amp.amp_mods_by_id.contains(id)) {
        val md5 = amp.amp_mods_by_id(id).head.md5
        md5s = Buffer(md5)
      }
    } else if (meta.url.contains("://amp.dascene.net/modules/")) {
      // url should have been decoded already
      val path = meta.url
        .toLowerCase
        .replaceAll("http[s]?://amp.dascene.net/modules//?","")
        .replace("//","/")
      if (sources.amp_by_path.contains(path)) {
        val md5 = sources.amp_by_path(path).head.md5
        md5s = Buffer(md5)
      }
    } else if (meta.url.contains("://aminet.net/") || meta.url.contains(".aminet.net/")) {
      val path = meta.url
        .toLowerCase
        .replaceAll("http[s]?://.*aminet.net/package//?","")
        .replaceAll("http[s]?:/.*aminet.net//?","")
        .replaceAll("ftp://.*aminet.net//?","")
        .replace("pub/aminet/", "")
        .replace("aminet/", "")
        .replace("//","/")
        .replace(".lzx","")
        .replace(".lha","")
      md5s = sources.findArchive(path, aminet_by_path).map(_._1).sorted.distinct
    } else if (meta.url.contains("://files.exotica.org.uk/?file=jpvs_scene_releases/") ||
               meta.url.contains("://malus.exotica.org.uk/pub/jpvs_scene_releases/")
    ) {
      val path = meta.url
        .toLowerCase
        .replaceAll("http[s]?://files.exotica.org.uk/\\?file=jpvs_scene_releases//?","")
        .replaceAll("http[s]?://malus.exotica.org.uk/pub/jpvs_scene_releases//?","")
        .replace("//","/")
      md5s = sources.findArchive(path, jpvscenereleases_by_path).map(_._1).sorted.distinct
    } else if (meta.url.contains("://defacto2.net/file/download/") ||
              (meta.url.contains("://defacto2.net/d/") && meta.url.contains("?filename="))) {
      val path = meta.url
        .toLowerCase
        .replaceAll("http[s]?://defacto2.net/d//?","defacto2.net/d/")
        .replaceAll("http[s]?://defacto2.net/file/download//?","defacto2.net/d/")
        .replace("//","/")
        .replaceAll("defacto2.net/d/.*\\?filename=","defacto2.net/d/")
      md5s = sources.findArchive(path, demozoo_prod_leftovers_by_path).map(_._1).sorted.distinct
    } else {
      val path = meta.url
        .toLowerCase
        .replaceAll("http[s]?://","")
        .replaceAll("ftp://","")
        .replace("//","/")
      md5s = sources.findArchive(path, demozoo_prod_leftovers_by_path).map(_._1).sorted.distinct
    }
    md5s.distinct.map((_, (meta, md5s.distinct)))
  }
}.flatten.seq.groupBy(_._1).mapValues(_.map(_._2)).par.flatMap { case (md5, _metas) =>
  val mindate = _metas.map(m => date(m._1.date, m._1.datePrecision, false)).min
  val metas = _metas.filter(m => date(m._1.date, m._1.datePrecision, false) <= mindate).flatMap { case (meta, md5s) =>
    var authors = Buffer.empty[String]
    val soundtracks = soundtracksByProdId.getOrElse(meta.id, Seq.empty)
    val prodMusicAuthors = meta.musicAuthors.filter(_.trim.nonEmpty).sorted.distinct
    val soundtrackAuthors = soundtracks.flatMap(_.authors).filter(_.trim.nonEmpty).sorted.distinct
    val allAuthors = prodMusicAuthors.union(soundtrackAuthors).sorted.distinct.filterNot(a => a.trim.isEmpty || a.trim == "?")
    if (md5s.size > 1 && md5s.size > soundtracks.size) {
      println(s"DEMOZOO EXTRA: multiple MD5s ${md5s} for meta ${meta} with soundtracks ${soundtracks}, skipping author matching")
    } else if (allAuthors.size <= 2 && (soundtracks.isEmpty || soundtracks.forall(s => s.authors.sorted.distinct == allAuthors))) {
      authors = allAuthors.toBuffer
    }
    var publishers = Buffer.empty[String]
    var album = ""
    var year = 0
    var _type = ""
    var _platform = ""
    val countBefore = soundtracks.count(f => date(f.modDate, f.modDatePrecision, false).take(4) < date(meta.date, meta.datePrecision, false).take(4))
    val countAfter = soundtracks.count(f => date(f.modDate, f.modDatePrecision, false).take(4) > date(meta.date, meta.datePrecision, false).take(4))
    val minSoundtrackDate = if (soundtracks.isEmpty) "9999-99-99" else soundtracks.map(s => date(s.modDate, s.modDatePrecision, false)).min
    if ((soundtracks.isEmpty && allAuthors.size <= 2) || ((countAfter >= countBefore || minSoundtrackDate.take(4).toInt >= date(meta.date, meta.datePrecision, false).take(4).toInt - 2) && (allAuthors.size <= 5 || countBefore == 0) && allAuthors.size <= soundtracks.size + 2)) {
      album = meta.title.trim
      publishers = meta.publishers.filter(_.trim.nonEmpty).sorted.distinct.toBuffer
      year = meta.date.take(4).toIntOption.getOrElse(0)
      _type = meta.prodType.sorted.headOption.getOrElse("")
      val normPlatforms = meta.platforms.map(normalizePlatform).sorted.distinct
      _platform = if (normPlatforms.isEmpty || normPlatforms.size > 1) "" else normPlatforms.head
    } else {
      //println(s"DEMOZOO EXTRA: skipping album/publisher/year/type/platform for md5 ${md5} meta ${meta} with soundtracks ${soundtracks}, allAuthors ${allAuthors.size}, countBefore ${countBefore}, countAfter ${countAfter}, minSoundtrackDate ${minSoundtrackDate}")
    }
    if (authors.nonEmpty || publishers.nonEmpty || album.nonEmpty || (year > 0 && year < 9999)) {
      Some((meta.id, MetaData(
        hash = md5.take(12),
        authors = authors,
        album = album,
        publishers = publishers,
        year = if (year > 0 && year < 9999) year else 0,
        _type = _type,
        _platform = _platform
      )))
    } else None
  }
  if (metas.isEmpty) {
    println(s"DEMOZOO EXTRA: no meta for MD5 ${md5} metas: ${_metas}")
    None
  } else {
    val scoredMetas = metas.map(e =>
      (e, (if (e._2._platform.toLowerCase == "amiga") 1 else 0) + (if (e._2._type.toLowerCase == "game") 1 else 0) + (if (e._2.authors.nonEmpty) 1 else 0) + (if (e._2.publishers.nonEmpty) 1 else 0) + (if (e._2.album.nonEmpty) 1 else 0) + (if (e._2.year > 0) 1 else 0))
    )
    val bestscore = scoredMetas.map(_._2).max
    val bestMetasForScore = scoredMetas.filter(_._2 == bestscore).map(_._1).toSeq

    // Fallback sorting for the "best" entry
    val SORT = "\u0001"
    val bestMeta = bestMetasForScore.sortBy(m => ("" +
     (if (m._2._type.isEmpty) SEPARATOR else if (m._2._type.toLowerCase == "game") 0 else 1) + SORT +
     (if (m._2._platform.isEmpty) SEPARATOR else if (m._2._platform.toLowerCase == "amiga") 0 else 1) + SORT +
     (if (m._2.year == 0) 9999 else m._2.year) + SORT +
     (if (m._2.authors.isEmpty) SEPARATOR else (10 - m._2.authors.size) + m._2.authors.mkString(SEPARATOR)) + SORT +
     (if (m._2.album.isEmpty) SEPARATOR else m._2.album) + SORT +
     (if (m._2.publishers.isEmpty) SEPARATOR else (10 - m._2.publishers.size) + m._2.publishers.mkString(SEPARATOR)) + SORT
    )).head

    Some(bestMeta)
  }
}.seq.toBuffer.distinct

for ((id, meta) <- demozooExtras) {
  println(s"DEMOZOO EXTRA META: $id - ${meta}")
}
