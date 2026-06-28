// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import java.util.concurrent.ConcurrentHashMap
import scala.collection.Map
import scala.collection.Set
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSet
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala
import scala.util.boundary, boundary.break

import audio._
import chromaprint._
import convert._
import normalization._
import demozoo._
import retroexo._
import songlengths._
import tosec._
import whdload._
import wikipedia._
import kestra._

def trace(msg: Unit => String): Unit = {
  //System.err.println(s"TRACE: ${msg(())}")
}

def debug(msg: String): Unit = {
  System.err.println(s"DEBUG: $msg")
}

def warn(msg: String): Unit = {
  System.err.println(s"WARN: $msg")
}

def removeArticle(s: String): String = {
  if (s.startsWith("The ")) s.substring(4)
  else if (s.startsWith("An ")) s.substring(3)
  else if (s.startsWith("A ")) s.substring(2)
  else s
}
def expandArticleVariants(m: MetaData): Set[MetaData] = {
  // Generate all combinations of article variants
  // For authors/publishers: if any item has an article, create versions with original and without
  val authorsLists = if (m.authors.isEmpty) {
    Seq(m.authors)
  } else {
    val authors = Seq(m.authors)
    val noArticles = m.authors.map(removeArticle)
    if (m.authors != noArticles) authors ++ Seq(noArticles)
    else authors
  }
  
  var albumLists = Seq(m.album)
  val albumNoArticle = removeArticle(m.album)
  if (m.album != albumNoArticle) albumLists = albumLists ++ Seq(albumNoArticle)
  
  val publishersLists = if (m.publishers.isEmpty) {
    Seq(m.publishers)
  } else {
    val publishers = Seq(m.publishers)
    val noArticles = m.publishers.map(removeArticle)
    if (m.publishers != noArticles) publishers ++ Seq(noArticles)
    else publishers
  }
  
  val variants = (for {
    authors <- authorsLists
    album <- albumLists
    publishers <- publishersLists
  } yield m.copy(hash = "", authors = authors, album = album, publishers = publishers))
  .filterNot(v => v.authors == m.authors && v.album == m.album && v.publishers == m.publishers)
  .toSet

  variants + m
}

val unnormalizedAuthors = amp.composer_handles.keys.par.flatMap(name => {
  val normalized = normalizeName(name)
  if (normalized != name) Some(normalized -> name) else None
}).toMap

val realNameVariants = (unexotica.composer_handles.keys ++ amp.composer_handles.keys ++ kestra.composer_handles.keys).par.flatMap(n => Seq(n, normalizeName(n)).distinct).flatMap(n => (Seq(n) ++ generateNameVariants(n))).toSet

def isRealName(a: String): Boolean = {
  realNameVariants.contains(a) || unnormalizedAuthors.contains(a)
}

def hasRealNames(authors: Buffer[String]): Boolean = {
  authors.exists(isRealName)
}

val authorVariantsCache = new ConcurrentHashMap[String, Buffer[String]]().asScala
def getAuthorVariants(a: String, knownAuthors: Set[String]): Buffer[String] = {
  if (a.isEmpty) return Buffer.empty
  if (authorVariantsCache.contains(a)) return authorVariantsCache(a)
  val normA = normalizeAuthor(a)
  var v = Buffer(a)
  
  val amp_all = amp.all_aliases.getOrElse(normA, Buffer.empty).filter(knownAuthors.contains)
  val kestra_all = kestra.all_aliases.getOrElse(normA, Buffer.empty).filter(knownAuthors.contains)
  val demozoo_all = demozoo.all_aliases.getOrElse(normA, Buffer.empty).filter(knownAuthors.contains)
  val unexotica_all = unexotica.all_aliases.getOrElse(normA, Buffer.empty).filter(knownAuthors.contains)
  val heads = (Seq(amp_all.headOption, kestra_all.headOption, demozoo_all.headOption, unexotica_all.headOption).flatten).distinct

  v ++= heads
  v ++= amp_all.filterNot(heads.contains)
  v ++= kestra_all.filterNot(heads.contains)
  v ++= demozoo_all.filterNot(heads.contains)
  v ++= unexotica_all.filterNot(heads.contains)

  authorVariantsCache(a) = v.distinct
  v
}

def areAuthorsCompatible(as1: Buffer[String], as2: Buffer[String], knownAuthors: Set[String]): Boolean = {
  if (as1.isEmpty || as2.isEmpty) return true
  val s1 = as1.groupBy(normalizeAuthor).map(_._2.head).toSeq
  val s2 = as2.groupBy(normalizeAuthor).map(_._2.head).toSeq
  val (smaller, larger) = if (s1.size <= s2.size) (s1, s2) else (s2, s1)

  smaller.exists { a1 =>
    val v1 = getAuthorVariants(a1, knownAuthors)
    larger.exists { a2 =>
      val v2 = getAuthorVariants(a2, knownAuthors)
      v1.exists(v2.contains)
    }
  }
}

def haveCompatibleAuthors(authorsList: Buffer[Buffer[String]], knownAuthors: Set[String]): Boolean = {
  if (authorsList.isEmpty) return true
  val distinctAuthors = authorsList.distinct
  var components = scala.collection.mutable.Buffer(scala.collection.mutable.Set(distinctAuthors.head))
  distinctAuthors.tail.foreach { a =>
    val matchingComponents = components.filter(c => c.exists(c_a => areAuthorsCompatible(a, c_a, knownAuthors)))
    if (matchingComponents.isEmpty) {
      components += scala.collection.mutable.Set(a)
    } else {
      val merged = matchingComponents.flatten.toSet + a
      components --= matchingComponents
      components += scala.collection.mutable.Set(merged.toSeq*)
    }
  }
  components.size == 1
}

def expandAuthorVariants(m: MetaData, knownAuthors: Set[String]): Set[MetaData] = {
  if (m.authors.isEmpty) return Set(m)

  val originalAuthors = m.authors.toSeq
  val combinations = originalAuthors.indices.flatMap { i =>
    val a = originalAuthors(i)
    // Pick one canonical case representation per normalized variant
    val variants = getAuthorVariants(a, knownAuthors).groupBy(normalizeAuthor).map(_._2.head)
    variants.map(v => originalAuthors.updated(i, v))
  }

  (Seq(originalAuthors) ++ combinations).map { newAuthors =>
    if (newAuthors == originalAuthors) m
    else m.copy(authors = newAuthors.toBuffer, hash = "")
  }.toSet
}

def pickMostCommonPublishers(metas: Iterable[MetaData]): Buffer[String] = {
  val grouped = metas.filter(_.publishers.nonEmpty).groupBy(_.publishers.sorted)
  if (grouped.nonEmpty) {
    grouped.maxBy(_._2.size)._1
  } else {
    Buffer.empty
  }
}

def pickMostCommonYear(metas: Iterable[MetaData]): Int = {
  val years = metas.filter(_.year != 0).map(_.year).seq
  if (years.nonEmpty) {
    val grouped = years.groupBy(identity)
    val mostCommon = grouped.maxBy(_._2.size)._1
    val maxCount = grouped(mostCommon).size
    val tiedYears = grouped.filter(_._2.size == maxCount).keys
    if (tiedYears.size == 1) mostCommon
    else tiedYears.min
  } else 0
}

def removeCompilations(metas: Buffer[MetaData]) = {
  // Find publisher/album combinations with more than 5 distinct authors
  val invalidAuthors = metas
    .filterNot(m => m.publishers.isEmpty && m.album.isEmpty)
    .groupBy(m => (m.publishers.sorted.mkString(","), m.album))
    .filter { case (_, metas) =>
      val authors = metas.flatMap(_.authors).filter(_.nonEmpty).distinct
      (metas.size >= 5 && authors.isEmpty) || authors.size >= 5
    }
    .keySet

  // Remove publisher/album from those compilations
  metas.map { meta =>
    val key = (meta.publishers.sorted.mkString(","), meta.album)
    if (invalidAuthors.contains(key)) {
      meta.copy(publishers = Buffer.empty, album = "", _type = "", _platform = "", year = 0)
    } else {
      meta
    }
  }
}

// calculate how many different sources a hash appears in
val sourceCounts = sources.tsvs.par.flatMap { case (source, metas) =>
  metas.map { case (md5, entries) => (md5.take(12), source) }
}.groupBy(_._1).mapValues(_.map(_._2).toSet.size).seq.toMap
val md5Constraints = sources.tsvs.par.flatMap { case (source, metas) =>
  val sourceConstraints = sources.sourceConstraints.get(source).getOrElse(Seq.empty)
  metas.map { case (md5, entries) => (md5.take(12), (entries.map(_.path), sourceConstraints, source)) }
}.groupBy(_._1).map { case (md5, values) =>
  val candidates = values.flatMap { case (_, (paths, constraints, source)) =>
    paths.flatMap { path =>
      constraints.find(c =>
        path.toLowerCase.startsWith(c.path.toLowerCase))
        .orElse(constraints.find(_.path.isEmpty))
      .map { constraint =>
        val maxYear = if (constraint.year > 0) constraint.year else Int.MaxValue
        val _type = constraint._type
        val _platform = constraint._platform
        (maxYear, _type, _platform, source)
      }
    }
  }
  val (maxYear0, source0) = sources.sourceYearConstraints.getOrElse(md5, (Int.MaxValue, sources.Source.NONE))
  val maxYear = if (candidates.nonEmpty) ((candidates.map(e => if (e._1 > 0) e._1 else Int.MaxValue)) ++ Seq(maxYear0)).min else maxYear0
  val typeCounts = candidates.filter(_._2.nonEmpty).groupBy(_._2).mapValues(_.size)
  val platformCounts = candidates.filter(_._3.nonEmpty).groupBy(_._3).mapValues(_.size)
  var sources_ = candidates.filter(e => e._1 == maxYear).map(_._4).toSet ++ candidates.filter(e => e._2.nonEmpty && e._2 == typeCounts.maxBy(_._2)._1).map(_._4).toSet ++ candidates.filter(e => e._3.nonEmpty && e._3 == platformCounts.maxBy(_._2)._1).map(_._4).toSet
  if (maxYear0 == maxYear) sources_ = sources_ ++ Set(source0)
  (md5, ((if (maxYear == 0) Int.MaxValue else maxYear, if (typeCounts.size == 1 && typeCounts.values.head > 1) typeCounts.keys.head else "", if (platformCounts.size == 1 && platformCounts.values.head > 1) platformCounts.keys.head else "", sources_)))
}
.filter { case (_, (maxYear, _type, platform, _)) => maxYear > 0 || _type.nonEmpty || platform.nonEmpty }
.seq.toMap

def filterByConstraints(meta: MetaData): Option[MetaData] = {
  if (meta.hash.isEmpty || meta.year == 0) return Some(meta)
  val (maxYear, _type, _platform, sources) = md5Constraints.getOrElse(meta.hash, (Int.MaxValue, "", "", Set.empty))
  if (maxYear == Int.MaxValue) return Some(meta)
  var filtered = meta
  val _maxYear = if (meta._type == "Compo") maxYear + 2 else maxYear + 1
  if (filtered.year > _maxYear) {
    warn(s"Invalid year ${meta.year} for ${meta}, max year from sources is ${_maxYear} sources: ${sources.mkString(", ")}")
    filtered = filtered.copy(year = 0, album = "", publishers = Buffer.empty, _type = "", _platform = "")
  }
  if (filtered.year > 0 || filtered.authors.nonEmpty || filtered.publishers.nonEmpty || filtered.album.nonEmpty ||
      filtered._type.nonEmpty || filtered._platform.nonEmpty) {
    if (filtered != meta) {
      warn(s"Filtered meta for ${meta.hash}: ${meta} -> ${filtered}")
    }
    Some(filtered)
  } else {
    warn(s"Filtered out meta for ${meta.hash} with no valid data: ${meta}")
    None
  }
}

def filterByConstraints(metas: Buffer[MetaData]): Buffer[MetaData] =
  metas.par.flatMap(filterByConstraints).seq.toBuffer

def filterByCracktros(m: MetaData, cracktros: Map[String, Set[MetaData]], games: Map[String, Set[MetaData]]): MetaData = {
    if (m._type.toLowerCase == "game" && m.year > 0 &&
      !m.album.endsWith(" AGA") && !m.album.contains("(AGA)")) {
      var matchingCracktros = cracktros.get(normalizeAlbum(m)).getOrElse(Set.empty).filter(c => (c._platform.isEmpty || m._platform.isEmpty || c._platform == m._platform) && _normalizeAlbum(c.album).startsWith(_normalizeAlbum(m.album)))
      val cracktroMaxYear = matchingCracktros.filter(_.year > 0).map(_.year).maxOption.getOrElse(0)
      val earlierCracktros = matchingCracktros.filter(_.year + 1 < m.year)
      val laterCracktros = matchingCracktros.filter(c => c.year >= m.year && c.year <= m.year + 1)
      val _games = games.getOrElse(normalizeAlbum(m), Set.empty)
        .filter(g => g._platform.isEmpty || m._platform.isEmpty || g._platform == m._platform)
      val authorMatch = _games
        .forall(g => m.authors.isEmpty || m.authors.exists(ma => g.authors.exists(ga => normalizeAuthor(ma) == normalizeAuthor(ga))) || m.authors.exists(ma => g.authors.exists(ga => normalizeAuthor(ma) == normalizeAuthor(ga))))
      val publisherMatch = _games
        .forall(g => m.publishers.isEmpty || m.publishers.exists(mp => g.publishers.exists(gp => normalizePublisher(mp) == normalizePublisher(gp))) || m.publishers.exists(mp => g.publishers.exists(gp => normalizePublisher(mp) == normalizePublisher(gp))))
      if (earlierCracktros.nonEmpty && laterCracktros.isEmpty && (cracktroMaxYear > 0 && m.year - cracktroMaxYear <= 1) && (authorMatch || publisherMatch)) {
        warn(s"Filtering year and publishers for ${m} due to earlier cracktros: ${earlierCracktros}, later cracktros: ${laterCracktros}, all matching cracktros: ${matchingCracktros} games: ${_games}")
        m.copy(year = 0)
      } else m
    } else m
}
def filterByCracktros(metas: Buffer[MetaData], cracktros: Map[String, Set[MetaData]], games: Map[String, Set[MetaData]]): Buffer[MetaData] =
  metas.par.map(m => filterByCracktros(m, cracktros, games)).seq.toBuffer

val __extras = kestra.kestraExtras.map(_._2) ++ demozoo.demozooExtras.map(_._2) ++ whdload.whdloadExtras.map(_._2) ++ retroexo.exodosExtras.map(_._2)
val _extras = __extras.groupBy(_.hash).par.flatMap { case (_, metas) =>
  val minYear = metas.filter(_.year > 0).map(_.year).minOption.getOrElse(0)
  val _metas = if (minYear > 0) metas.filter(m => m.year <= minYear + 1) else metas
  val scoredMetas = _metas.map(e =>
    (e, (if (e._platform.toLowerCase == "amiga" || e._type == "Compo") 1 else 0) + (if (e._type.toLowerCase == "game") 1 else 0) + (if (e.authors.nonEmpty) 1 else 0) + (if (e.publishers.nonEmpty || e._type == "Tool") 1 else 0) + (if (e.album.nonEmpty || e._type == "Compo") 1 else 0) + (if (e.year > 0) 1 else 0) + (if (e.year <= minYear) 1 else 0))
  )
  val bestscore = scoredMetas.map(_._2).max
  val bestMetasForScore = scoredMetas.filter(_._2 == bestscore).map(_._1).toSeq

  // Fallback sorting for the "best" entry
  val SORT = "\u0001"
  val bestMeta = bestMetasForScore.sortBy(m => ("" +
   (if (m._type.isEmpty) SEPARATOR else if (m._type.toLowerCase == "game") 0 else 1) + SORT +
   (if (m._platform.isEmpty) SEPARATOR else if (m._platform.toLowerCase == "amiga" || m._type == "Compo") 0 else 1) + SORT +
   (if (m.year == 0) 9999 else m.year) + SORT +
   (if (m.authors.isEmpty) SEPARATOR else (10 - m.authors.size) + m.authors.mkString(SEPARATOR)) + SORT +
   (if (m.album.isEmpty) SEPARATOR else m.album) + SORT +
   (if (m.publishers.isEmpty) SEPARATOR else (10 - m.publishers.size) + m.publishers.mkString(SEPARATOR)) + SORT
  )).head

  Some(bestMeta)
}.seq.toBuffer

def combineMetadata(
  _amp: Buffer[MetaData],
  _modland: Buffer[MetaData],
  _unexotica: Buffer[MetaData],
  _demozoo: Buffer[MetaData],
  _kestra: Buffer[MetaData],
  _oldexotica: Buffer[MetaData],
  _wantedteam: Buffer[MetaData],
  _modsanthology: Buffer[MetaData],
  _fujiology: Buffer[MetaData],
  _tosecmusic: Buffer[MetaData], // only supplementary
  _leftovers: Buffer[MetaData], // only supplementary
): Buffer[MetaData] = {
  val hashes = (
    _amp.par.map(_.hash) ++
    _modland.par.map(_.hash) ++
    _unexotica.par.map(_.hash) ++
    _demozoo.par.map(_.hash) ++
    _kestra.par.map(_.hash) ++
    _oldexotica.par.map(_.hash) ++
    _wantedteam.par.map(_.hash) ++
    _modsanthology.par.map(_.hash) ++
    _fujiology.par.map(_.hash) ++
    _tosecmusic.par.map(_.hash) ++
    _extras.par.map(_.hash) ++
    sources.sourcePathYears.par.map(_.hash)
  ).toSet

  var extraMetas = Set.empty[MetaData] ++ (
    (_amp ++ _modland ++ _unexotica ++ _kestra ++ _demozoo ++ _oldexotica ++ _wantedteam ++ _modsanthology ++ _fujiology ++ _leftovers ++ _extras).map(_.copy(hash = ""))
    ++
    tosecMetas ++ whdloadMetas ++ demozooMetas.map(_._2) ++ kestraMetas.map(_._2) ++ exodosMetas ++ wikipediaMetas)

  val tmpMetas = extraMetas.filter(e => e.publishers.nonEmpty && e.album.nonEmpty).map(e => (normalizeAlbum(e), e.publishers.map(normalizePublisher).distinct, e.year)).toSet
  extraMetas = extraMetas ++
    // XXX unreliable metadata filter out conflicting ones
    _tosecmusic.par.map(m => {
      if (m.album.nonEmpty && m.publishers.nonEmpty) {
        val normAlbum = normalizeAlbum(m)
        val normPublishers = m.publishers.map(normalizePublisher)
        var publishers = m.publishers
        var year = m.year
        val metas = tmpMetas.filter(e => e._1 == normAlbum)
        if (year != 0 && metas.exists(e => e._2.exists(normPublishers.contains) && e._3 != m.year && e._3 != 0)) {
          trace(_ => s"TOSEC MUSIC: ${m} conflicting year ${m.year} vs other sources, removing year")
          year = 0
        }
        if (metas.exists(e => normPublishers.exists(!e._2.contains(_)))) {
          trace(_ => s"TOSEC MUSIC: ${m} conflicting publishers ${m.publishers.mkString(", ")} vs other sources, removing publishers")
          publishers = Buffer.empty
        }
        m.copy(hash = "", publishers = publishers, year = year)
      } else m.copy(hash = "")
    })
    .seq
  extraMetas = extraMetas
    .par
    .filterNot(m => (m._type == "Game" && m._platform == "PC" && (m.year > 0 && m.year <= 1991)))
    .filterNot(m => (m._platform == "PC" && (m.year > 0 && m.year < 1990)))
    .filterNot(m => (m._platform == "Atari" && (m.year > 0 && m.year < 1988)))
    // XXX
    .filterNot(m => (m._type == "Game" && m._platform == "PC" && m.album == "The Thing" && m.year == 2002))
    .filterNot(m => m.authors.isEmpty && m.album.isEmpty && m.publishers.isEmpty) // only year
    .filterNot(m => m.authors.isEmpty && m.album.isEmpty && m.year == 0) // only publishers
    .filterNot(m => m.authors.isEmpty && m.publishers.isEmpty && m.year == 0) // only album
    //.filterNot(m => m.album.isEmpty && m.publishers.isEmpty && m.year == 0) // only authors
    // XXX
    .map(m =>
      if (m.authors == Buffer("Juselius Mamies")) m.copy(authors = Buffer("Juselius Maamies"))
      else m
    )
    .seq

  val games = extraMetas.par.filter(m => m._type.toLowerCase == "game" && m.year > 0)
    .seq.groupBy(normalizeAlbum)
  val nongames = extraMetas.par.filter(m => m._type.nonEmpty && m._type.toLowerCase != "game" && m.year > 0)
    .seq.groupBy(normalizeAlbum)
  val cracktros = extraMetas.par.filter(m => isCracktro(m._type) && m.year > 0)
    // XXX wrong year
    .filterNot(c => c.album.toLowerCase == "toado" && c.year == 1991)
    .seq.groupBy(normalizeAlbum)
  extraMetas = extraMetas.par.map(filterByCracktros(_, cracktros, games)).seq

  val Seq(amp, modland, unexotica, demozoo, kestra, oldexotica, wantedteam, modsanthology, fujiology, extras) =
    Seq(_amp, _modland, _unexotica, _demozoo, _kestra, _oldexotica, _wantedteam, _modsanthology, _fujiology, _extras,
    // _tosemusic, _leftovers
    ).par.map(s => filterByCracktros(filterByConstraints(s), cracktros, games)).seq

  val fujiology2 = removeCompilations(fujiology)

  val demozoog = demozoo.groupBy(_.hash).par.mapValues(v => v.head.copy(
    album = v.head.album
    .replaceAll(" - Different Version$", "")
  )).seq
  val kestrag = kestra.groupBy(_.hash).par.mapValues(_.head).seq
  val ampg = amp.groupBy(_.hash).par.mapValues(_.head).seq
  // canonize Falcon (PL) -> Falcon etc.
  val modlandg = modland.groupBy(_.hash).par.mapValues(v => v.head.copy(
    authors = v.head.authors.map(_.replaceAll(" \\(.*\\)$", "")))).seq
  val unexoticag = unexotica.groupBy(_.hash).par.mapValues(_.head).seq
  val oldexoticag = oldexotica.groupBy(_.hash).par.mapValues(_.head).seq
  val wantedteamg = wantedteam.groupBy(_.hash).par.mapValues(_.head).seq
  // canonize XXX of YYY -> XXX
  // XXX.sweden -> XXX etc.
  val modsanthologyg = modsanthology.groupBy(_.hash).par.mapValues(v => v.head.copy(
    authors = v.head.authors.map(_
    .replaceAll(" of .*", "")
    .replaceAll("\\.canada$", "")
    .replaceAll("\\.denmark$", "")
    .replaceAll("\\.finland$", "")
    .replaceAll("\\.france$", "")
    .replaceAll("\\.germany$", "")
    .replaceAll("\\.norway$", "")
    .replaceAll("\\.quebec$", "")
    .replaceAll("\\.sweden$", "")
    .replaceAll("\\.uk$", "")
    .replaceAll("\\.usa$", "")
  ))).seq
  val fujiologyg = fujiology2.groupBy(_.hash).par.mapValues(_.head).seq
  val extrasg = extras.groupBy(_.hash).par.mapValues(_.head).seq
  val sourcePathYearsg = filterByConstraints(sources.sourcePathYears).groupBy(_.hash).par.mapValues(_.head).seq
  //val tosecmusicg = tosecmusic.groupBy(_.hash).par.mapValues(_.head).seq // too unreliable

  // authors: AMP > Demozoo > Modland > UnExotica > OldExotica > WantedTeam > ModsAnthology > Fujiology
  val authorSources = Seq(
    ampg,
    unexoticag,
    demozoog,
    kestrag,
    modlandg,
    oldexoticag,
    wantedteamg,
    modsanthologyg,
    fujiologyg,
    extrasg,
    // tosecmusicg, // too unreliable
  )

  val authenticAuthorMetas = (authorSources
    .par
    .flatMap(_.values.filter(m => m.authors.nonEmpty).map(m => (m.authors.map(normalizeAuthor).sorted.distinct, m)))
    ++
    extraMetas.par.filter(m => m.authors.nonEmpty).map(m => (m.authors.map(normalizeAuthor).sorted.distinct, m)))
    .par
    .groupBy(_._1)
    .map { case (authors, pairs) => authors -> pairs.map(_._2).seq.toSet }
    .seq

  val knownAuthors = extraMetas.par.flatMap(_.authors).seq.toSet
  
  val nonSceneGroups = Set("Binary Emotions", "Diamond Software", "Edge", "Frontier Software", "Imageworks", "Kalisto", "New Deal", "Ocean", "Ocean Software","Psygnosis", "Rainbow Arts", "Starbyte", "Thalion","Unique Development Sweden")
  val sceneGroups = extraMetas.par.filter(m => m._type.nonEmpty && m._type.toLowerCase != "game" && m.album.nonEmpty && m.publishers.nonEmpty).flatMap(_.publishers).seq.toSet
    .filterNot(nonSceneGroups.contains)

  val uniqueAlbumTypes = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._type.nonEmpty && !isCracktro(m._type))
    .groupBy(m => normalizeAlbum(m))
    .filter { case (_, metas) => metas.map(_._type).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => (normalizeAlbum(m), m._type)) }
    .seq.toMap

  val _uniqueAlbumTypes = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._type.nonEmpty && !isCracktro(m._type))
    .groupBy(m => _normalizeAlbum(m.album))
    .filter { case (_, metas) => metas.map(_._type).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => (_normalizeAlbum(m.album), m._type)) }
    .seq.toMap

  val uniqueAlbumPlatforms = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._platform.nonEmpty && !isCracktro(m._type))
    .groupBy(m => normalizeAlbum(m))
    .filter { case (_, metas) => metas.map(_._platform).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => (normalizeAlbum(m), m._platform)) }
    .seq.toMap

  val _uniqueAlbumPlatforms = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._platform.nonEmpty && !isCracktro(m._type))
    .groupBy(m => _normalizeAlbum(m.album))
    .filter { case (_, metas) => metas.map(_._platform).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => (_normalizeAlbum(m.album), m._platform)) }
    .seq.toMap

  val uniqueAlbumTypePlatformToYear = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._type.nonEmpty && m._platform.nonEmpty && !isCracktro(m._type) && m.year > 0)
    .groupBy(m => (normalizeAlbum(m), normalizeType(m._type), m._platform))
    .filter { case (_, metas) => metas.map(_.year).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => ((normalizeAlbum(m), normalizeType(m._type), m._platform), m.year)) }
    .seq.toMap

  val _uniqueAlbumTypePlatformToYear = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._type.nonEmpty && m._platform.nonEmpty && !isCracktro(m._type) && m.year > 0)
    .groupBy(m => (_normalizeAlbum(m.album), normalizeType(m._type), m._platform))
    .filter { case (_, metas) => metas.map(_.year).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => ((_normalizeAlbum(m.album), normalizeType(m._type), m._platform), m.year)) }
    .seq.toMap

  val uniqueAlbumTypePlatformPublishersToYear = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._type.nonEmpty && m._platform.nonEmpty && m.publishers.nonEmpty && !isCracktro(m._type) && m.year > 0)
    .groupBy(m => (normalizeAlbum(m), normalizeType(m._type), m._platform, m.publishers.map(normalizePublisher).sorted.distinct))
    .filter { case (_, metas) => metas.map(_.year).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => ((normalizeAlbum(m), normalizeType(m._type), m._platform, m.publishers.map(normalizePublisher).sorted.distinct), m.year)) }
    .seq.toMap

  val _uniqueAlbumTypePlatformPublishersToYear = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._type.nonEmpty && m._platform.nonEmpty && m.publishers.nonEmpty && !isCracktro(m._type) && m.year > 0)
    .groupBy(m => (_normalizeAlbum(m.album), normalizeType(m._type), m._platform, m.publishers.map(normalizePublisher).sorted.distinct))
    .filter { case (_, metas) => metas.map(_.year).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => ((_normalizeAlbum(m.album), normalizeType(m._type), m._platform, m.publishers.map(normalizePublisher).sorted.distinct), m.year)) }
    .seq.toMap

  val uniqueAlbumTypePlatformYearToPublishers = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._type.nonEmpty && m._platform.nonEmpty && m.year > 0 && !isCracktro(m._type))
    .groupBy(m => (normalizeAlbum(m), normalizeType(m._type), m._platform, m.year))
    .filter { case (_, metas) => metas.map(_.publishers.map(normalizePublisher).toSet).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.toBuffer.sortBy(-_.publishers.map(_.length).sum).headOption.map(m => ((normalizeAlbum(m), normalizeType(m._type), m._platform, m.year), m.publishers)) }
    .seq.toMap

  val _uniqueAlbumTypePlatformYearToPublishers = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._type.nonEmpty && m._platform.nonEmpty && m.year > 0 && !isCracktro(m._type))
    .groupBy(m => (_normalizeAlbum(m.album), normalizeType(m._type), m._platform, m.year))
    .filter { case (_, metas) => metas.map(_.publishers.map(normalizePublisher).toSet).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.toBuffer.sortBy(-_.publishers.map(_.length).sum).headOption.map(m => ((_normalizeAlbum(m.album), normalizeType(m._type), m._platform, m.year), m.publishers)) }
    .seq.toMap

  val uniqueAlbumPublishersPlatformTypeToAuthors = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m.publishers.nonEmpty && m._platform.nonEmpty && m._type.nonEmpty)
    .groupBy(m => (normalizeAlbum(m), m.publishers.map(normalizePublisher).sorted.distinct, m._platform, normalizeType(m._type)))
    .filter { case (_, metas) => metas.map(_.authors.toSet).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => ((normalizeAlbum(m), m.publishers.map(normalizePublisher).sorted.distinct, m._platform, normalizeType(m._type)), m.authors)) }
    .seq.toMap

  val _uniqueAlbumPublishersPlatformTypeToAuthors = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m.publishers.nonEmpty && m._platform.nonEmpty && m._type.nonEmpty)
    .groupBy(m => (_normalizeAlbum(m.album), m.publishers.map(normalizePublisher).sorted.distinct, m._platform, normalizeType(m._type)))
    .filter { case (_, metas) => metas.map(_.authors.toSet).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => ((_normalizeAlbum(m.album), m.publishers.map(normalizePublisher).sorted.distinct, m._platform, normalizeType(m._type)), m.authors)) }
    .seq.toMap

  val uniqueAlbumYearPlatformTypeToAuthors = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m.year > 0 && m._platform.nonEmpty && m._type.nonEmpty)
    .groupBy(m => (normalizeAlbum(m), m.year, m._platform, normalizeType(m._type)))
    .filter { case (_, metas) => metas.map(_.authors.toSet).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => ((normalizeAlbum(m), m.year, m._platform, normalizeType(m._type)), m.authors)) }
    .seq.toMap

  val _uniqueAlbumYearPlatformTypeToAuthors = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m.year > 0 && m._platform.nonEmpty && m._type.nonEmpty)
    .groupBy(m => (_normalizeAlbum(m.album), m.year, m._platform, normalizeType(m._type)))
    .filter { case (_, metas) => metas.map(_.authors.toSet).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => ((_normalizeAlbum(m.album), m.year, m._platform, normalizeType(m._type)), m.authors)) }
    .seq.toMap

  val uniqueAlbumTypeToPlatform = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._type.nonEmpty && m._platform.nonEmpty)
    .groupBy(m => (normalizeAlbum(m), normalizeType(m._type)))
    .filter { case (_, metas) => metas.map(_._platform).toSet.size == 1 && metas.map(_.year).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => ((normalizeAlbum(m), normalizeType(m._type)), m._platform)) }
    .seq.toMap

  val _uniqueAlbumTypeToPlatform = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._type.nonEmpty && m._platform.nonEmpty)
    .groupBy(m => (_normalizeAlbum(m.album), normalizeType(m._type)))
    .filter { case (_, metas) => metas.map(_._platform).toSet.size == 1 && metas.map(_.year).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => ((_normalizeAlbum(m.album), normalizeType(m._type)), m._platform)) }
    .seq.toMap

  val uniqueAlbumPlatformToType = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._type.nonEmpty && m._platform.nonEmpty)
    .groupBy(m => (normalizeAlbum(m), m._platform))
    .filter { case (_, metas) => metas.map(_._type).toSet.size == 1 && metas.map(_.year).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => ((normalizeAlbum(m), m._platform), normalizeType(m._type))) }
    .seq.toMap

  val _uniqueAlbumPlatformToType = extraMetas
    .par
    .filter(m => m.album.nonEmpty && m._type.nonEmpty && m._platform.nonEmpty)
    .groupBy(m => (_normalizeAlbum(m.album), m._platform))
    .filter { case (_, metas) => metas.map(_._type).toSet.size == 1 && metas.map(_.year).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => ((_normalizeAlbum(m.album), m._platform), normalizeType(m._type))) }
    .seq.toMap

  val uniqueAlbumGameToYear = extraMetas
    .par
    .filter(m => m.album.nonEmpty && normalizeType(m._type) == "Game" && m.year > 0)
    // XXX
    .filterNot(m =>
      m.album == "Reaxxion" ||
      m.album == "Bubsy: Paws on Fire!" ||
      m.album.toLowerCase == "bloodnet" ||
      m.album == "Robbo"
    )
    .groupBy(normalizeAlbum)
    .filter { case (_, metas) => metas.map(_.year).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => (normalizeAlbum(m), m.year)) }
    .seq.toMap

  val _uniqueAlbumGameToYear = extraMetas
    .par
    .filter(m => m.album.nonEmpty && normalizeType(m._type) == "Game" && m.year > 0)
    // XXX
    .filterNot(m =>
      m.album == "Reaxxion" ||
      m.album == "Bubsy: Paws on Fire!" ||
      m.album.toLowerCase == "bloodnet" ||
      m.album == "Robbo"
    )
    .groupBy(m => _normalizeAlbum(m.album))
    .filter { case (_, metas) => metas.map(_.year).toSet.size == 1 }
    .flatMap { case (_, metas) => metas.map(m => (_normalizeAlbum(m.album), m.year)) }
    .seq.toMap

  def deduceMetas(hash: String, authors: Buffer[String], album: String, publishers: Buffer[String], year: Int, _type: String, _platform: String, processAgain: Boolean): (Buffer[String], Buffer[String], Int, String, String) = {
    var (a, al, p, y, t, pl) = (authors, album, publishers, year, _type, _platform)
    lazy val normAlbum = normalizeAlbum(_type, album, publishers, year)
    lazy val _normAlbum = _normalizeAlbum(album)
    lazy val normPublishers = p.map(normalizePublisher).sorted.distinct
    var normType = normalizeType(t)
    lazy val gameType = normType == "Game" && (p.isEmpty || !p.forall(sceneGroups.contains))
    if (pl.isEmpty && (processAgain || gameType)) {
      val entries = songlengths.songlengthsByMd5(hash)
      if (entries.exists(e =>
        e.format.endsWith("ST") ||
        e.format.contains(" ST ") ||
        e.format.contains("TCB Tracker") ||
        e.format.contains("YM2149") ||
        e.format.contains(" PSG") ||
        e.format.contains("Octalyser") ||
        e.format.contains("Graoumf") ||
        e.format.contains("Digital Tracker") ||
        e.format.contains("Megatracker")
      )) {
        pl = "Atari"
      } else if (entries.exists(e =>
        e.format.contains("DigiBooster") ||
        e.format.contains("DIGI Booster") ||
        e.format.contains("OctaMED") ||
        e.format.contains("MED ") ||
        e.format.contains("Future Composer") ||
        e.format.contains("Face The Music") ||
        e.format.contains("Symphonie") ||
        e.format.contains("UNIC Tracker") ||
        e.format.contains("SoundFX") ||
        e.format.toLowerCase.contains("chiptracker") ||
        e.format.contains("Images Music System") ||
        e.format.contains("Ice Tracker") ||
        e.format.contains("Game Music Creator") ||
        e.format.contains("Startrekker") ||
        e.format.contains("IFFMODL") ||
        e.format.contains("Slamtilt") ||
        e.format.contains("Magnetic Fields Packer") ||
        e.format.contains("His Master's Noise") ||
        e.format.contains("AMOS Music Bank") ||
        e.format.contains("Prorunner") ||
        e.format.contains("The Player") ||
        e.format.contains("ProPacker") ||
        e.format.contains("NoisePacker") ||
        e.format.contains("Kefrens") ||
        e.format.contains("Titanics") ||
        e.format.contains("Pha Packer") ||
        e.format.contains("Wanton Packer") ||
        e.format.contains("Fuchs Tracker") ||
        e.format.contains("Eureka Packer") ||
        e.format.contains("Promizer") ||
        e.format.contains("AC1D Packer") ||
        e.format.contains("The Dark Demon")
      )) {
        pl = "Amiga"
      } else if (!entries.exists(e =>
        e.format.toLowerCase.contains("soundtracker") ||
        e.format.toLowerCase.contains("noisetracker") ||
        e.format.toLowerCase.contains("protracker") ||
        e.format.toLowerCase.contains("oktalyzer") ||
        e.format.endsWith("SID") ||
        e.format.contains("POKEYNoise") ||
        e.format.contains("Archimedes Tracker") ||
        e.format.contains("Coconizer") ||
        e.format.contains("Blade Packer") ||
        e.format == "Jochen Hippel" || e.format == "Jochen Hippel COSO" || // also used on Atari
        e.format == "PumaTracker" || // also used on Atari
        e.format == "TFMX" || e.format == "TFMX Pro" ||// also used on Atari
        e.format == "Howie Davies" || // also used on Atari
        e.format == "Special FX" || // also used on Atari
        e.player == "hivelytracker" ||
        e.player == "ft2play"
      )) {
        if (entries.exists(_.player == "uade")) {
          pl = "Amiga"
        } else if (!entries.forall(_.player.isEmpty)) {
          pl = "PC"
        }
      }
      if (pl.nonEmpty) {
        debug(s"DEDUCED PLATFORM ${pl} for ${hash} based on formats: ${entries.map(_.format).distinct.mkString(", ")} players: ${entries.map(_.player).distinct.mkString(", ")}")
      }
      if (pl.isEmpty && al.nonEmpty && t.nonEmpty) {
        pl = uniqueAlbumTypeToPlatform.getOrElse((normAlbum, normalizeType(t)), "")
        if (pl.nonEmpty) {
          debug(s"DEDUCED platform '${pl}' for album '${al}', type '${t}' hash ${hash}")
        } else {
          pl = _uniqueAlbumTypeToPlatform.getOrElse((_normAlbum, normalizeType(t)), "")
          if (pl.nonEmpty) {
            debug(s"DEDUCED platform '${pl}' for album '${al}', type '${t}' hash ${hash}")
          }
        }
      }
      if (pl.isEmpty && al.nonEmpty) {
        pl = uniqueAlbumPlatforms.getOrElse(normAlbum, "")
        if (pl.nonEmpty) {
          debug(s"DEDUCED platform '${pl}' for album '${al}' hash ${hash}")
        } else {
          pl = _uniqueAlbumPlatforms.getOrElse(_normAlbum, "")
          if (pl.nonEmpty) {
            debug(s"DEDUCED platform '${pl}' for album '${al}' hash ${hash}")
          }
        }
      }
    }
    if (processAgain) {
      if (t.isEmpty && al.nonEmpty && pl.nonEmpty) {
        t = uniqueAlbumPlatformToType.getOrElse((normAlbum, pl), "")
        if (t.nonEmpty) {
          debug(s"DEDUCED type '${t}' for album '${al}', platform '${pl}' hash ${hash}")
        } else {
          t = _uniqueAlbumPlatformToType.getOrElse((_normAlbum, pl), "")
          if (t.nonEmpty) {
            debug(s"DEDUCED type '${t}' for album '${al}', platform '${pl}' hash ${hash}")
          }
        }
      }
      if (t.isEmpty && al.nonEmpty && processAgain) {
        t = uniqueAlbumTypes.getOrElse(normAlbum, "")
        if (t.nonEmpty) {
          debug(s"DEDUCED type '${t}' for album '${al}' hash ${hash}")
        } else {
          t = _uniqueAlbumTypes.getOrElse(_normAlbum, "")
          if (t.nonEmpty) {
            debug(s"DEDUCED type '${t}' for album '${al}' hash ${hash}")
          }
        }
      }
    }
    normType = normalizeType(t)
    if (y == 0 && al.nonEmpty && (t.nonEmpty || pl.nonEmpty) && (processAgain || gameType)) {
      if (t.nonEmpty && pl.nonEmpty) {
        y = uniqueAlbumTypePlatformToYear.getOrElse((normAlbum, normType, pl), 0)
      }
      if (y == 0 && t.nonEmpty && gameType) {
        y = uniqueAlbumGameToYear.getOrElse(normAlbum, 0)
      }
      if (y != 0 && t.nonEmpty && pl.nonEmpty) {
        val normPubs = uniqueAlbumTypePlatformYearToPublishers.getOrElse((normAlbum, normType, pl, y), Buffer.empty).map(normalizePublisher).sorted.distinct
        val pubsMatch = normPublishers.isEmpty || normPubs.isEmpty || normPubs.exists(normPublishers.contains) || normPublishers.exists(normPubs.contains)
        if (!pubsMatch) {
          y = 0
        }
      }
      if (y == 0) {
        if (t.nonEmpty && pl.nonEmpty) {
          y = _uniqueAlbumTypePlatformToYear.getOrElse((_normAlbum, normType, pl), 0)
        }
        if (y == 0 && t.nonEmpty && gameType) {
          y = _uniqueAlbumGameToYear.getOrElse(_normAlbum, 0)
        }
        if (y != 0 && t.nonEmpty && pl.nonEmpty) {
          val normPubs = _uniqueAlbumTypePlatformYearToPublishers.getOrElse((_normAlbum, normType, pl, y), Buffer.empty).map(normalizePublisher).sorted.distinct
          val pubsMatch = normPublishers.isEmpty || normPubs.isEmpty || normPubs.exists(normPublishers.contains) || normPublishers.exists(normPubs.contains)
          if (!pubsMatch) {
            y = 0
          }
        }
      }
      if (y != 0) {
        debug(s"DEDUCED year '${y}' for album '${al}', type '${t}', platform '${pl}' hash ${hash}")
      }
    }
    if (y == 0 && al.nonEmpty && t.nonEmpty && pl.nonEmpty && p.nonEmpty) {
      y = uniqueAlbumTypePlatformPublishersToYear.getOrElse((normAlbum, normType, pl, normPublishers), 0)
      if (y != 0) {
        debug(s"DEDUCED year '${y}' for album '${al}', type '${t}', platform '${pl}', publishers '${p.mkString(", ")}' hash ${hash}")
      } else {
        y = _uniqueAlbumTypePlatformPublishersToYear.getOrElse((_normAlbum, normType, pl, normPublishers), 0)
        if (y != 0) {
          debug(s"DEDUCED year '${y}' for album '${al}', type '${t}', platform '${pl}', publishers '${p.mkString(", ")}' hash ${hash}")
        }
      }
    }
    if (p.isEmpty && y != 0 && al.nonEmpty && t.nonEmpty && pl.nonEmpty) {
      p = uniqueAlbumTypePlatformYearToPublishers.getOrElse((normAlbum, normType, pl, y), Buffer.empty)
      if (p.nonEmpty) {
        debug(s"DEDUCED publishers '${p.mkString(", ")}' for album '${al}', type '${t}', platform '${pl}', year '${y}' hash ${hash}")
      } else {
        p = _uniqueAlbumTypePlatformYearToPublishers.getOrElse((_normAlbum, normType, pl, y), Buffer.empty)
        if (p.nonEmpty) {
          debug(s"DEDUCED publishers '${p.mkString(", ")}' for album '${al}', type '${t}', platform '${pl}', year '${y}' hash ${hash}")
        }
      }
    }
    if (a.isEmpty && al.nonEmpty && p.nonEmpty && t.nonEmpty && pl.nonEmpty) {
      a = uniqueAlbumPublishersPlatformTypeToAuthors.getOrElse((normAlbum, normPublishers, pl, normType), Buffer.empty)
      if (a.nonEmpty) {
        debug(s"DEDUCED authors '${a.mkString(", ")}' for album '${al}', publishers '${p.mkString(", ")}',platform '${pl}', type '${t}' hash ${hash}")
      } else {
        a = _uniqueAlbumPublishersPlatformTypeToAuthors.getOrElse((_normAlbum, normPublishers, pl, normType), Buffer.empty)
        if (a.nonEmpty) {
          debug(s"DEDUCED authors '${a.mkString(", ")}' for album '${al}', publishers '${p.mkString(", ")}', platform '${pl}', type '${t}' hash ${hash}")
        }
      }
    }
    if (a.isEmpty && al.nonEmpty && y != 0 && t.nonEmpty && pl.nonEmpty) {
      a = uniqueAlbumYearPlatformTypeToAuthors.getOrElse((normAlbum, y, pl, normType), Buffer.empty)
      if (a.nonEmpty) {
        debug(s"DEDUCED authors '${a.mkString(", ")}' for album '${al}', year '${y}', platform '${pl}', type '${t}' hash ${hash}")
      } else {
        a = _uniqueAlbumYearPlatformTypeToAuthors.getOrElse((_normAlbum, y, pl, normType), Buffer.empty)
        if (a.nonEmpty) {
          debug(s"DEDUCED authors '${a.mkString(", ")}' for album '${al}', year '${y}', platform '${pl}', type '${t}' hash ${hash}")
        }
      }
    }
    (a, p, y, t, pl)
  }

  extraMetas = extraMetas
    .par
    .flatMap(expandArticleVariants)
    .flatMap(m => expandAuthorVariants(m, knownAuthors))
    .seq

  var metasByHash = new ConcurrentHashMap[String, MetaData]().asScala

  // album/publishers/year source priority
  val allMetaSources = Seq(
    unexoticag.filter(_._2._type.toLowerCase == "game"),
    kestrag.filter(_._2._type.toLowerCase == "game"),
    oldexoticag.filter(_._2._type.toLowerCase == "game"),
    wantedteamg.filter(_._2._type.toLowerCase == "game"),
    ampg.filter(_._2._type.toLowerCase == "game"),
    fujiologyg.filter(e => e._2._type.toLowerCase == "game" && e._2._platform.toLowerCase != "atari"),
    extrasg.filter(_._2._type.toLowerCase == "game"),
    demozoog.filterNot(e => e._2._type.toLowerCase == "musicdisk" && e._2._platform.toLowerCase == "atari"),
    kestrag.filter(_._2._type.toLowerCase != "game"),
    oldexoticag.filter(_._2._type.toLowerCase != "game"),
    unexoticag.filter(_._2._type.toLowerCase != "game"),
    wantedteamg.filter(_._2._type.toLowerCase != "game"),
    ampg.filter(_._2._type.toLowerCase != "game"),
    modlandg,
    modsanthologyg,
    demozoog.filter(e => e._2._type.toLowerCase == "musicdisk" && e._2._platform.toLowerCase == "atari"),
    fujiologyg.filterNot(e => e._2._type.toLowerCase == "game" && e._2._platform.toLowerCase != "atari"),
    extrasg.filter(_._2._type.toLowerCase != "game"),
    sourcePathYearsg
  )

  for (pass <- 1 to allMetaSources.size + 1) {
    val processAgain = pass == allMetaSources.size + 1

    def trace(msg: Unit => String): Unit = {
      //System.err.println(s"TRACE ($pass): ${msg(())}")
    }
    def debug(msg: String): Unit = {
      System.err.println(s"DEBUG ($pass): $msg")
    }
    def info(msg: String): Unit = {
      System.err.println(s"INFO ($pass): $msg")
    }
    def warn(msg: String): Unit = {
      System.err.println(s"WARN ($pass): $msg")
    }

    val _pass = Math.min(pass, allMetaSources.size)
    val metaSources = allMetaSources.take(_pass)
    val excludedHashes = allMetaSources.drop(_pass).flatMap(_.keys).toSet -- metaSources.flatMap(_.keys)
    hashes.par.foreach { hash =>
      val existing = metasByHash.get(hash)
 
      def pickAuthor[T](sources: Seq[Map[String, MetaData]]) =
        if (existing.isDefined && existing.get.authors.nonEmpty) {
          Some(existing.get.authors)
        } else {
          sources.map(_.get(hash)).find(m =>
            m.isDefined && m.get.authors.nonEmpty).map(_.get.authors)
        }

      def pick[T](sources: Seq[Map[String, MetaData]], f: MetaData => T): Option[T] = {
        val _all = sources.flatMap(_.get(hash)).filter(m =>
          // pick only if has some non-author metadata
          m.publishers.nonEmpty || m.album.nonEmpty || m.year != 0)
        val minyear = _all.filter(_.year != 0).map(_.year).minOption.getOrElse(0)
        def _score (e: MetaData): Int = (if (e.publishers.nonEmpty || e._type == "Tool") 10 else 0) + (if (e.album.nonEmpty || e._type == "Compo") 1000 else 0) + (if (e.year > 0) 10000 else 0) + (if (e._platform.toLowerCase == "amiga" || e._type == "Compo") 1 else 0) + (if (e._type.toLowerCase == "game") 1 else 0) + (if (e.year > 0 && e.year <= minyear) 1 else 0)
        val all = _all.filter(m => minyear == 0 || m.year <= minyear + 1 || (m._type == "Compo" && m.year <= minyear + 2)).map(e => {
          val pScore = _score(e)
          (e, pScore)
        })
        val bestscore = if (all.nonEmpty) all.map(_._2).max else 0
        val best = all.filter(_._2 >= bestscore).headOption
        val picked = best.map(_._1)
        val pScore = best.map(_._2).getOrElse(0)
        val eScore = if (existing.isDefined) existing.map(e => _score(e)).get else 0

        if (existing.isDefined && (existing.get.publishers.nonEmpty || existing.get.album.nonEmpty || existing.get.year != 0) && (!picked.isDefined ||
        (pScore < eScore && (picked.get.year == 0 || picked.get.year >= existing.get.year - 1)) ||
        (pScore == eScore && (picked.get.year == 0 || picked.get.year >= (if (existing.get._type.toLowerCase == "game" && (existing.get.publishers.isEmpty || !existing.get.publishers.exists(sceneGroups.contains))) existing.get.year - 1 else existing.get.year))) ||
        (pScore > eScore && (existing.get.year != 0 && existing.get.year + 1 < picked.get.year))))
          existing.map(f)
        else {
          // XXX AMP album-only special case
          if (existing.isDefined && existing.get.album.nonEmpty && picked.isDefined && picked.get.album.isEmpty) {
            val e = existing.get
            val p = picked.get
            lazy val pickedWithExistingAlbum =
              picked.map(_.copy(publishers = e.publishers, album = e.album, _type = e._type, _platform = e._platform)).map(f)
            if ((e.year == 0 || p.year == 0 || e.year == p.year) &&
                (e._type.isEmpty || p._type.isEmpty || normalizeType(e._type) == normalizeType(p._type)) &&
                (e._platform.isEmpty || p._platform.isEmpty || e._platform.toLowerCase == p._platform.toLowerCase) &&
                (e.publishers.isEmpty || p.publishers.isEmpty || e.publishers.exists(ep => p.publishers.exists(pp => normalizePublisher(ep) == normalizePublisher(pp))))) {
              if (e._type.toLowerCase == "game" && p.year > 0 && e.year == 0) {
                if (games.getOrElse(normalizeAlbum(e), Set.empty).filter(g => g._platform.isEmpty || e._platform.isEmpty || g._platform.toLowerCase == e._platform.toLowerCase).exists(g => g.year == 0 || g.year == p.year))
                  pickedWithExistingAlbum
                else
                  picked.map(f)
              } else if (e._type.nonEmpty && e._type.toLowerCase != "game" && p.year > 0 && e.year == 0) {
                if (nongames.getOrElse(normalizeAlbum(e), Set.empty).filter(g => g._platform.isEmpty || e._platform.isEmpty || g._platform.toLowerCase == e._platform.toLowerCase).exists(g => g.year == 0 || g.year == p.year))
                  pickedWithExistingAlbum
                else
                  picked.map(f)
              } else pickedWithExistingAlbum
          } else picked.map(f)
          } else picked.map(f)
        }
      }

      var authors = pickAuthor(authorSources).getOrElse(Buffer.empty)
      var album = pick(metaSources, f = _.album).getOrElse("")
      var publishers = pick(metaSources, f = _.publishers).getOrElse(Buffer.empty)
      var year = pick(metaSources, f = _.year).getOrElse(0)
      var _type = pick(metaSources, f = _. _type).getOrElse("")
      var _platform = pick(metaSources, f = _. _platform).getOrElse("")

      deduceMetas(hash, authors, album, publishers, year, _type, _platform, processAgain) match {
        case (a, p, y, t, pl) =>
          authors = a
          publishers = p
          year = y
          _type = t
          _platform = pl
      }

      if (publishers.isEmpty && existing.isDefined && existing.get.publishers.nonEmpty && album.isEmpty && existing.get.album.isEmpty && year != 0 && existing.get.year == 0) {
        publishers = existing.get.publishers
      }

      if (year == 0 && existing.isDefined && existing.get.year != 0 && album.isEmpty && existing.get.album.isEmpty && publishers.nonEmpty && existing.get.publishers.isEmpty) {
        year = existing.get.year
      }

      var updated = MetaData(hash, authors, publishers, album, year, _type, _platform)

      lazy val passesConstraint = filterByConstraints(updated.copy(hash = hash, authors = Buffer.empty)).isDefined
      if (existing.isDefined && existing.get != updated && passesConstraint) {
        val typeChange = (existing.get._type.toLowerCase == "game" && updated._type.toLowerCase != "game") || (existing.get._type.toLowerCase != "game" && updated._type.toLowerCase == "game")
        if (typeChange) {
          var isGame = updated._type.toLowerCase == "game"
          var newAuthors = authorSources.map(_.get(hash)).find(m =>
            m.isDefined && (if (isGame) m.get._type.toLowerCase == "game" else m.get._type.toLowerCase != "game") && m.get.authors.nonEmpty).map(_.get.authors).getOrElse(Buffer.empty)
          if (newAuthors.isEmpty) {
            val metaSource = metaSources.last.get(hash)
            newAuthors = if (metaSource.isDefined && metaSource.get.authors.nonEmpty) metaSource.get.authors else existing.get.authors
          }
          updated = updated.copy(authors = newAuthors)
        }
      }

      if (updated != MetaData(hash, Buffer.empty, Buffer.empty, "", 0, "", "") && (existing.isEmpty || existing.get != updated) && passesConstraint) {
        debug(s"initial pick for $hash -> ${updated}")
        metasByHash(hash) = updated
      } else if (authors.nonEmpty && (existing.isEmpty || existing.get.authors != authors)) { 
        debug(s"initial author pick for $hash -> ${authors.mkString(", ")}")
        metasByHash(hash) =
          if (existing.isEmpty) MetaData(hash, authors, Buffer.empty, "", 0, "", "")
          else existing.get.copy(authors = authors)
      }
    }
  
    val allmetas = metasByHash.values ++ extraMetas

    val authorMetas = allmetas
      .par
      .filterNot(_.authors.isEmpty)
      .filterNot(m => m.year == 0 && m.publishers.isEmpty && m.album.isEmpty && m._type.isEmpty && m._platform.isEmpty)
      .flatMap(m => {
        m.authors.map(normalizeAuthor).flatMap { author =>
          Set(
            (author, m)
          )
        }
      })
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)

    val yearAlbumPublishers = allmetas
      .par
      .filterNot(_.album.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.year, normalizeAlbum(m), m.publishers.map(normalizePublisher).sorted.distinct))
      .seq.toBuffer.distinct
  
    val yearAlbum = allmetas
      .par
      .filterNot(_.album.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.year, normalizeAlbum(m)))
      .seq.toBuffer.toSet

    val yearUniqueAlbumPublishersPlatformType = allmetas
      .par
      .filterNot(_.album.isEmpty)
      .filterNot(_.year == 0)
      .filter(m => m._type.nonEmpty && !isCracktro(m._type))
      .filterNot(_._platform.isEmpty)
      .groupBy(m => (normalizeAlbum(m), normalizeType(m._type), m._platform))
      .filter(m => m._2.map(_.year).toSet.size == 1 && m._2.map(_.publishers.map(normalizePublisher)).filterNot(_.isEmpty).toSet.size <= 1)
      .flatMap { case (_, metas) => metas.map(m => (m.year, normalizeAlbum(m), normalizeType(m._type), m._platform)) }
      .seq.toBuffer.toSet
    
    val albumPublishers = allmetas
      .par
      .filterNot(_.album.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .map(m => (normalizeAlbum(m), m.publishers.map(normalizePublisher).sorted.distinct))
      .seq.toBuffer.distinct

    val authorsAlbumPublishers = allmetas
      .par
      .filterNot(_.authors.isEmpty)
      .filterNot(_.album.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .map(m => (m.authors.map(normalizeAuthor).sorted.distinct, normalizeAlbum(m), m.publishers.map(normalizePublisher).sorted.distinct))
      .seq.toBuffer.distinct
  
    val authorsAlbumYear = allmetas
      .par
      .filterNot(_.authors.isEmpty)
      .filterNot(_.album.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.authors.map(normalizeAuthor).sorted.distinct, normalizeAlbum(m), m.year))
      .seq.toBuffer.distinct

    val authorsYearPublishers = allmetas
      .par
      .filterNot(_.authors.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.authors.map(normalizeAuthor).sorted.distinct, m.year, m.publishers.map(normalizePublisher).sorted.distinct))
      .seq.toBuffer.distinct

    val authorsYearNoAlbum = allmetas
      .par
      .filter(_.album.isEmpty)
      .filterNot(_.authors.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.authors.map(normalizeAuthor).sorted.distinct, m.year))
      .seq.toBuffer.distinct

    val authorsPublishersNoAlbum = allmetas
      .par
      .filter(_.album.isEmpty)
      .filterNot(_.authors.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .map(m => (m.authors.map(normalizeAuthor).sorted.distinct, m.publishers.map(normalizePublisher).sorted.distinct))
      .seq.toBuffer.distinct

    metasByHash.values.par.foreach { meta =>
      val hash = meta.hash
      var authors = meta.authors
      var album = meta.album
      var publishers = meta.publishers
      var year = meta.year
      var _type = meta._type
      var _platform = meta._platform

      val normPublishers = publishers.map(normalizePublisher)
      val normAuthors = authors.flatMap(a => getAuthorVariants(a, knownAuthors)).map(normalizeAuthor)


      def pickYearAlbumWithPublishers(m: Option[MetaData]) = {
        if (year == 0 && album.isEmpty && m.isDefined && m.get.year != 0 && m.get.album.nonEmpty) {
          if (publishers.nonEmpty && yearAlbumPublishers.exists(e => e._1 == m.get.year &&
              e._2 == normalizeAlbum(m.get) &&
              e._3.exists(normPublishers.contains)) &&
              !publishers.head.endsWith(" Party") &&
              _type != "Compo"
          ) {
            debug(s"pickYearAlbumWithPublishers: $hash -> ${m.get.year} + ${m.get.album}")
            year = m.get.year
            album = m.get.album
          }
        }
      }

      def pickYearAlbumWithoutPublishers(m: Option[MetaData]) = {
        if (year == 0 && album.isEmpty && m.isDefined && m.get.year != 0 && m.get.album.nonEmpty) {
          if (publishers.isEmpty && yearAlbum.contains((m.get.year, normalizeAlbum(m.get)))) {
            debug(s"pickYearAlbumWithoutPublishers: $hash -> ${m.get.year} + ${m.get.album}")
            year = m.get.year
            album = m.get.album
            _type = m.get._type
            _platform = m.get._platform
          }
        }
      }

      def pickAlbumPublishersWithYear(m: Option[MetaData]) = {
        if (album.isEmpty && publishers.isEmpty && m.isDefined && m.get.album.nonEmpty && m.get.publishers.nonEmpty) {
          val mNormPubs = m.get.publishers.map(normalizePublisher)
          val mNormAlb = normalizeAlbum(m.get)
          if (year != 0 && yearAlbumPublishers.exists(e => e._1 == year &&
              e._2 == mNormAlb &&
              e._3.exists(mNormPubs.contains))
          ) {
            debug(s"pickAlbumPublishersWithYear: $hash -> ${m.get.album} + ${m.get.publishers}")
            album = m.get.album
            publishers = m.get.publishers
            _type = m.get._type
            _platform = m.get._platform
          }
        }
      }

      def pickAlbumPublishersWithoutYear(m: Option[MetaData]) = {
        if (album.isEmpty && publishers.isEmpty && m.isDefined && m.get.album.nonEmpty && m.get.publishers.nonEmpty) {
          val mNormPubs = m.get.publishers.map(normalizePublisher)
          val mNormAlb = normalizeAlbum(m.get)
          if (year == 0 && albumPublishers.exists(e => e._1 == mNormAlb &&
              e._2.exists(mNormPubs.contains))
           ) {
            debug(s"pickAlbumPublishersWithoutYear: $hash -> ${m.get.album} + ${m.get.publishers}")
            album = m.get.album
            publishers = m.get.publishers
            _type = m.get._type
            _platform = m.get._platform
          }
        }
      }

      def pickAlbumWithPublishersAndYear(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty) {
          if (year != 0 && publishers.nonEmpty && yearAlbumPublishers.exists(e => e._1 == year &&
              e._2 == normalizeAlbum(m.get) &&
              e._3.exists(normPublishers.contains)) &&
              !publishers.head.endsWith(" Party") &&
              _type != "Compo"
          ) {
            debug(s"pickAlbumWithPublishersAndYear: $hash -> ${m.get.album}")
            album = m.get.album
            _type = m.get._type
            _platform = m.get._platform
          }
        }
      }

      def pickAlbumWithPublishers(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty) {
          if (year == 0 && publishers.nonEmpty && albumPublishers.exists(e =>
              e._1 == normalizeAlbum(m.get) &&
              e._2.exists(normPublishers.contains)) &&
              !publishers.head.endsWith(" Party") &&
              _type != "Compo"
          ) {
            debug(s"pickAlbumWithPublishers: $hash -> ${m.get.album}")
            album = m.get.album
            _type = m.get._type
            _platform = m.get._platform
          }
        }
      }

      def pickUniqueAlbumWithYearPlatformType(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty &&
           ((_type.nonEmpty && (m.get._type.isEmpty || normalizeType(m.get._type) == normalizeType(_type))) ||
            (m.get._type.nonEmpty && (_type.isEmpty || normalizeType(m.get._type) == normalizeType(_type)))) &&
            ((_platform.nonEmpty && (m.get._platform.isEmpty || m.get._platform.toLowerCase == _platform.toLowerCase)) ||
            (m.get._platform.nonEmpty && (_platform.isEmpty || m.get._platform.toLowerCase == _platform.toLowerCase)))
        ) {
          val type_ = if (_type.nonEmpty) _type else m.get._type
          val platform_ = if (_platform.nonEmpty) _platform else m.get._platform
          if (publishers.isEmpty && year != 0 && yearUniqueAlbumPublishersPlatformType.contains((year, normalizeAlbum(m.get), normalizeType(type_), platform_))) {
            debug(s"pickUniqueAlbumWithYearPlatformType: $hash -> ${m.get.album}")
            album = m.get.album
            _type = type_
            _platform = platform_
          }
        }
      }

      def pickPublishersWithAlbumAndYear(m: Option[MetaData]): Unit = {
        if (publishers.isEmpty && m.isDefined && m.get.publishers.nonEmpty) {
          val mNormPubs = m.get.publishers.map(normalizePublisher)
          val mNormAlbPub = normalizeAlbum(_type, album, m.get.publishers, year)
          if (year != 0 && album.nonEmpty && yearAlbumPublishers.exists(e => e._1 == year &&
              e._2 == mNormAlbPub &&
              e._3.exists(mNormPubs.contains))
          ) {
            debug(s"pickPublishersWithAlbumAndYear: $hash -> ${m.get.publishers}")
            publishers = m.get.publishers
          }
        }
      }

      def pickPublishersWithAlbum(m: Option[MetaData]): Unit = {
        if (publishers.isEmpty && m.isDefined && m.get.publishers.nonEmpty) {
          val mNormPubs = m.get.publishers.map(normalizePublisher)
          val mNormAlbPub = normalizeAlbum(_type, album, m.get.publishers, year)
          if (year == 0 && album.nonEmpty && albumPublishers.exists(e =>
              e._1 == mNormAlbPub &&
              e._2.exists(mNormPubs.contains))
          ) {
            debug(s"pickPublishersWithAlbum: $hash -> ${m.get.publishers}")
            publishers = m.get.publishers
          }
        }
      }

      def pickYearWithAlbumAndPublishers(m: Option[MetaData]) = {
        if (year == 0 && m.isDefined && m.get.year != 0) {
          if (album.nonEmpty && publishers.nonEmpty && yearAlbumPublishers.exists(e => e._1 == m.get.year &&
              e._2 == normalizeAlbum(_type, album, publishers, m.get.year) &&
              e._3.exists(normPublishers.contains)) &&
              !publishers.head.endsWith(" Party") &&
              _type != "Compo"
          ) {
            debug(s"pickYearWithAlbumAndPublishers: $hash -> ${m.get.year}")
            year = m.get.year
          }
        }
      }

      def pickYearWithUniqueAlbumPlatformType(m: Option[MetaData]) = {
        if (year == 0 && m.isDefined && m.get.year != 0 &&
           ((_type.nonEmpty && (m.get._type.isEmpty || normalizeType(m.get._type) == normalizeType(_type))) ||
            (m.get._type.nonEmpty && (_type.isEmpty || normalizeType(m.get._type) == normalizeType(_type)))) &&
            ((_platform.nonEmpty && (m.get._platform.isEmpty || m.get._platform.toLowerCase == _platform.toLowerCase)) ||
            (m.get._platform.nonEmpty && (_platform.isEmpty || m.get._platform.toLowerCase == _platform.toLowerCase)))
        ) {
          val type_ = if (_type.nonEmpty) _type else m.get._type
          val platform_ = if (_platform.nonEmpty) _platform else m.get._platform
          if (publishers.isEmpty && album.nonEmpty && yearUniqueAlbumPublishersPlatformType.contains((m.get.year, normalizeAlbum(_type, album, Buffer.empty, m.get.year), normalizeType(type_), platform_))) {
            debug(s"pickYearWithUniqueAlbumPlatformType: $hash -> ${m.get.year}")
            year = m.get.year
            _type = type_
            _platform = platform_
          }
        }
      }

      def pickAlbumWithAuthorsYearPublishers(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty) {
          if (authors.nonEmpty && year != 0 && publishers.nonEmpty &&
              authorsYearPublishers.exists(e =>
                e._1.exists(normAuthors.contains) &&
                e._2 == year &&
                e._3.exists(normPublishers.contains)
              ) &&
              authorsAlbumPublishers.exists(e =>
                e._1.exists(normAuthors.contains) &&
                e._2 == normalizeAlbum(m.get) &&
                e._3.exists(publishers.map(normalizePublisher).contains)
              ) &&
              authorsAlbumYear.exists(e =>
                e._1.exists(normAuthors.contains) &&
                e._2 == normalizeAlbum(m.get) &&
                e._3 == year) &&
              !publishers.head.endsWith(" Party") &&
              _type != "Compo"
          ) {
            debug(s"pickAlbumWithAuthorsYearPublishers: $hash -> ${m.get.album}")
            album = m.get.album
            _type = m.get._type
            _platform = m.get._platform
          }
        }
      }

      def pickAlbumWithAuthorsYear(m: Option[MetaData]) = {
        if (album.isEmpty && publishers.isEmpty && m.isDefined && m.get.album.nonEmpty && m.get.  publishers.isEmpty) {
          if (authors.nonEmpty && year == m.get.year && authorsAlbumYear.exists(e =>
              e._1.exists(normAuthors.contains) &&
              e._2 == normalizeAlbum(m.get) &&
              e._3 == year)
          ) {
            debug(s"pickAlbumWithAuthorsYear: $hash -> ${m.get.album}")
            album = m.get.album
            _type = m.get._type
            _platform = m.get._platform
          }
        }
      }

      def pickAlbumWithAuthorsPublishers(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty && m.get.year == year) {
          if (authors.nonEmpty && publishers.nonEmpty && authorsAlbumPublishers.exists(e =>
              e._1.exists(normAuthors.contains) &&
              e._2 == normalizeAlbum(m.get) &&
              e._3.exists(publishers.map(normalizePublisher).contains)) &&
              !publishers.head.endsWith(" Party") &&
              _type != "Compo"
          ) {
            debug(s"pickAlbumWithAuthorsPublishers: $hash -> ${m.get.album}")
            album = m.get.album
            _type = m.get._type
            _platform = m.get._platform
          }
        }
      }

      def pickYearPublishersWithoutAlbum(m: Option[MetaData]) = {
        if (year == 0 && publishers.isEmpty && album.isEmpty && m.isDefined && m.get.year != 0 && m.get.publishers.nonEmpty) {
          val mNormPubs = m.get.publishers.map(normalizePublisher)
          if (authors.nonEmpty && m.get.album.isEmpty &&
              authorsYearPublishers.exists(e =>
                e._1.exists(normAuthors.contains) &&
                e._2 == m.get.year &&
                e._3.exists(mNormPubs.contains)
              )
          ) {
            debug(s"pickYearPublishersWithoutAlbum: $hash -> ${m.get.year} + ${m.get.publishers}")
            year = m.get.year
            publishers = m.get.publishers
          }
        }
      }

      def pickYearWithoutAlbum(m: Option[MetaData]) = {
        if (year == 0 && album.isEmpty && m.isDefined && m.get.year != 0) {
          if (authors.nonEmpty && m.get.album.isEmpty &&
              authorsYearNoAlbum.exists(e =>
                e._1.exists(normAuthors.contains) &&
                e._2 == m.get.year)
          ) {
            debug(s"pickYearWithoutAlbum: $hash -> ${m.get.year}")
            year = m.get.year
          }
        }
      }

      def pickPublishersWithoutAlbum(m: Option[MetaData]) = {
        if (publishers.isEmpty && album.isEmpty && m.isDefined && m.get.publishers.nonEmpty && m.get.year == year) {
          val mNormPubs = m.get.publishers.map(normalizePublisher)
          if (authors.nonEmpty && m.get.album.isEmpty &&
              authorsPublishersNoAlbum.exists(e =>
                e._1.exists(normAuthors.contains) &&
                e._2.exists(mNormPubs.contains)
              )
          ) {
            debug(s"pickPublishersWithoutAlbum: $hash -> ${m.get.publishers}")
            publishers = m.get.publishers
          }
        }
      }

      val sources = metaSources.map(_.get(hash))

      sources.foreach(pickYearAlbumWithPublishers)
      sources.foreach(pickYearAlbumWithoutPublishers)

      sources.foreach(pickAlbumPublishersWithYear)
      sources.foreach(pickAlbumPublishersWithoutYear)
      sources.foreach(pickAlbumWithPublishersAndYear)
      sources.foreach(pickAlbumWithPublishers)
      sources.foreach(pickUniqueAlbumWithYearPlatformType)

      sources.foreach(pickPublishersWithAlbumAndYear)
      sources.foreach(pickPublishersWithAlbum)

      sources.foreach(pickYearWithAlbumAndPublishers)
      sources.foreach(pickYearWithUniqueAlbumPlatformType)

      sources.foreach(pickAlbumWithAuthorsYearPublishers)
      sources.foreach(pickAlbumWithAuthorsYear)
      sources.foreach(pickAlbumWithAuthorsPublishers)

      sources.foreach(pickYearPublishersWithoutAlbum)
      sources.foreach(pickYearWithoutAlbum)
      sources.foreach(pickPublishersWithoutAlbum)

      deduceMetas(hash, authors, album, publishers, year, _type, _platform, processAgain) match {
        case (a, p, y, t, pl) => 
           authors = a
           publishers = p
           year = y
           _type = t
           _platform = pl
      }

      val updated = MetaData(hash, authors, publishers, album, year, _type, _platform)
      if (meta != updated && filterByConstraints(updated.copy(hash = hash, authors = Buffer.empty)).isDefined) {
        debug(s"after pick for $hash -> ${updated}")
        metasByHash(hash) = updated
      }
    }
  
    // find metas which have common author(s) + album, add publishers and year if missing
    val metasByAuthorAlbumWithPublisherOrYear = (metasByHash.values ++ extraMetas)
      .par
      .filterNot(_.authors.isEmpty)
      .filterNot(_.album.isEmpty)
      .filterNot(m => m.publishers.isEmpty && m.year == 0)
      .groupBy(m => (
        m.authors.map(normalizeAuthor),
        normalizeAlbum(m)
      ))
      .toSeq
      .flatMap { case (key, metas) =>
        key._1.map(a => (a, key._2) -> metas)
      }
      .groupBy(_._1)
      .mapValues(_.flatMap(_._2).seq.toSet)
      .seq

    metasByHash.values.par.foreach(m =>
      if (!(m.authors.isEmpty || m.album.isEmpty || (m.publishers.nonEmpty && m.year != 0))) {
        val keys = m.authors.map(a => 
          (normalizeAuthor(a),
           normalizeAlbum(m))
        )
        trace(_ => s"(1) ${m.hash} keys: ${keys}")
        val key = keys.find(metasByAuthorAlbumWithPublisherOrYear.contains(_))
        val meta = if (key.isDefined) {
          var metas = metasByAuthorAlbumWithPublisherOrYear(key.get)
          if (m._type.nonEmpty) {
            val filtered = metas.filter(m2 => normalizeType(m._type) == normalizeType(m2._type))
            metas = if (filtered.nonEmpty) filtered else metas.filter(_._type.isEmpty)
          }
          if (m._platform.nonEmpty) {
            val filtered = metas.filter(m2 => m2._platform.toLowerCase == m._platform.toLowerCase)
            metas = if (filtered.nonEmpty) filtered else metas.filter(_._platform.isEmpty)
          }
          if (metas.filter(m2 => _normalizeAlbum(m2.album) == _normalizeAlbum(m.album)).nonEmpty) {
            metas = metas.filter(m2 => _normalizeAlbum(m2.album) == _normalizeAlbum(m.album))
          }
          var publishers = if (m.publishers.isEmpty) pickMostCommonPublishers(metas) else m.publishers
          val normPubs = publishers.map(normalizePublisher)
          if (!metas.forall(m => m.publishers.isEmpty
            || m.publishers.map(normalizePublisher).exists(normPubs.contains)
            || normPubs.exists(p => m.publishers.map(normalizePublisher).contains(p)))
          ) {
            warn(s"(1) publishers differ for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.publishers.mkString(",")} != ${metas.map(_.publishers.mkString(",")).mkString(" | ")} pubs: ${publishers.mkString(", ")} normPubs: ${normPubs.mkString(", ")} normMetasPubs: ${metas.map(_.publishers.map(normalizePublisher).mkString(",")).mkString(" | ")}")
          }
          // TODO tag source + exclude/override modsanthology year
          var year = if (m.year == 0) pickMostCommonYear(metas) else m.year
          if (!metas.forall(m => m.year == 0 || m.year == year)) {
            warn(s"(1) year differs for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.year} != ${metas.map(_.year).mkString(",")}")
            year = m.year
          }
          if (metas.nonEmpty && (publishers != m.publishers || year != m.year) && filterByConstraints(m.copy(authors = Buffer.empty, year = year)).isDefined) {
            debug(s"(1) overriding metadata for ${m} -  publishers ${m.publishers.mkString(",")} -> ${publishers.mkString(",")}, year ${m.year} -> ${year}")
            trace(_ => s"(1) ${m.hash} metas: ${metas.seq} key: ${key} allmetas: ${metasByAuthorAlbumWithPublisherOrYear.get(key.get)}")
            metasByHash(m.hash) = m.copy(publishers = publishers, year = year)
          }
        }
      }
    )
  
    // find metas which have common publisher(s) + album, add year if missing
    val metasByPublisherAlbumWithYear = (metasByHash.values ++ extraMetas)
      .par
      .filterNot(_.publishers.isEmpty)
      .filterNot(_.album.isEmpty)
      .filter(_.year != 0)
      .groupBy(m => (
        m.publishers.map(normalizePublisher),
        normalizeAlbum(m)
      ))
      .toSeq
      .flatMap { case (key, metas) =>
        key._1.map(p => (p, key._2) -> metas)
      }
      .groupBy(_._1)
      .mapValues(_.flatMap(_._2).seq.toSet)
      .seq

    metasByHash.values.par.foreach(m =>
      if (!(m.album.isEmpty || m.publishers.isEmpty || m.year != 0)) {
        val keys = m.publishers.map(p => 
          (normalizePublisher(p),
           normalizeAlbum(m))
        )
        trace(_ => s"(2) ${m.hash} keys: ${keys}")
        val key = keys.find(metasByPublisherAlbumWithYear.contains(_))
        if (key.isDefined) {
          var metas = metasByPublisherAlbumWithYear(key.get)
          if (m._type.nonEmpty) {
            val filtered = metas.filter(m2 => normalizeType(m._type) == normalizeType(m2._type))
            metas = if (filtered.nonEmpty) filtered else metas.filter(_._type.isEmpty)
          }
          if (m._platform.nonEmpty) {
            val filtered = metas.filter(m2 => m2._platform.toLowerCase == m._platform.toLowerCase)
            metas = if (filtered.nonEmpty) filtered else metas.filter(_._platform.isEmpty)
          }
          if (metas.filter(m2 => _normalizeAlbum(m2.album) == _normalizeAlbum(m.album)).nonEmpty) {
            metas = metas.filter(m2 => _normalizeAlbum(m2.album) == _normalizeAlbum(m.album))
          }
          // TODO tag source + exclude/override modsanthology year
          var year = if (m.year == 0) pickMostCommonYear(metas) else m.year
          if (!metas.forall(m => m.year == 0 || Math.abs(m.year - year) <= 1)) {
            warn(s"(2) year differs for ${m.hash} - ${m.album} - ${m.publishers.mkString(",")} - ${m.year} != ${metas.map(_.year).mkString(",")}")
          } else if (metas.nonEmpty && year != m.year && filterByConstraints(m.copy(authors = Buffer.empty, year = year)).isDefined) {
            debug(s"(2) overriding year for ${m} - year ${m.year} -> ${year}")
            trace(_ => s"(2) ${m.hash} metas: ${metas.seq} key: ${key} allmetas: ${metasByPublisherAlbumWithYear.get(key.get)}")
            metasByHash(m.hash) = m.copy(year = year)
          }
        }
      }
    )

    // if meta author is missing, compare to other metas
    // and when there is only 1 album with same non-empty name and only 1 distinct author(s) for that album and publisher matches (or is missing in the original meta)
    // -> add author, publisher and year
    val metasByAlbumWithAuthorPublisherOrYear = (metasByHash.values ++ extraMetas)
      .par
      .filterNot(m => m.album.isEmpty || Set("musicdisk","megademo","slideshow").exists(normalizeAlbum(m).startsWith(_)))
      .filterNot(m => m.publishers.isEmpty && m.year == 0)
      .groupBy(m => normalizeAlbum(m))
      .mapValues(_.seq)
      .seq

    metasByHash.values.par.foreach(m => {
      if (!(m.authors.nonEmpty || m.album.isEmpty || (m.publishers.nonEmpty && m.year != 0) ||
           (m.album.nonEmpty && m.authors.isEmpty && m.publishers.isEmpty && m.year == 0))) {
        val key = normalizeAlbum(m)
        trace(_ => s"(3) ${m.hash} key: ${key}")
        var metas = metasByAlbumWithAuthorPublisherOrYear.get(key)
        if (m._type.nonEmpty && metas.isDefined) {
          val filtered = metas.get.filter(m2 => normalizeType(m._type) == normalizeType(m2._type))
          metas = if (filtered.nonEmpty) Some(filtered) else Some(metas.get.filter(_._type.isEmpty))
        }
        if (m._platform.nonEmpty && metas.isDefined) {
          val filtered = metas.get.filter(m2 => m2._platform.toLowerCase == m._platform.toLowerCase)
          metas = if (filtered.nonEmpty) Some(filtered) else Some(metas.get.filter(_._platform.isEmpty))
        }
        if (metas.isDefined && metas.get.filter(m2 => _normalizeAlbum(m2.album) == _normalizeAlbum(m.album)).nonEmpty) {
          metas = Some(metas.get.filter(m2 => _normalizeAlbum(m2.album) == _normalizeAlbum(m.album)))
        }
        if (metas.isDefined && metas.get.size >= 1) {
          val authors = {
            val grouped = metas.get.groupBy(_.authors.sorted)
            grouped.maxBy(_._2.size)._1
          }
          if (metas.get.forall(m => areAuthorsCompatible(m.authors, authors, knownAuthors))) {
            var publishers = if (m.publishers.isEmpty) pickMostCommonPublishers(metas.get) else m.publishers
            val normPublishers = publishers.map(normalizePublisher)
            if (!metas.get.forall(m2 => m.publishers.isEmpty || m2.publishers.isEmpty
                || m2.publishers.map(normalizePublisher).exists(normPublishers.contains)
                || normPublishers.exists(p => m2.publishers.map(normalizePublisher).contains(p)))
            ) {
              warn(s"(3) publishers differ for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.publishers.mkString(",")} != ${metas.get.flatMap(_.publishers).mkString(",")}")
            } else {
              var year = if (m.year == 0) pickMostCommonYear(metas.get) else m.year
              if (!metas.get.forall(m => m.year == 0 || Math.abs(m.year - year) <= 1)) {
                warn(s"(3) year differs for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.year} != ${metas.get.map(_.year).mkString(",")}")
              } else if ((authors != m.authors || publishers != m.publishers || year != m.year) && filterByConstraints(m.copy(authors = Buffer.empty, year = year)).isDefined) {
                debug(s"(3) overriding metadata for ${m} - authors ${m.authors.mkString(",")} -> ${authors.mkString(",")}, publishers ${m.publishers.mkString(",")} -> ${publishers.mkString(",")}, year ${m.year} -> ${year}")
                trace(_ => s"(3) ${m.hash} metas: ${metas.get.seq} key: ${key} allmetas: ${metasByAlbumWithAuthorPublisherOrYear.get(key)}")
                metasByHash(m.hash) = m.copy(authors = authors, publishers = publishers, year = year)
              }
            }
          }
        }
      }
    })

    // fill/update metadatas using audio fingerprints
    // process twice to get metadata from "transitive" duplicates also (A matches B, B matches C, but A doesn't match C)

    def processAudioTag(audioTag: String, hashes: List[String]): Unit = {
      def _filter(m: MetaData): Boolean = {
        val filtered = filterByConstraints(m)
        filtered.isDefined && filtered.get == m
      }
      def keepAuthor(m: MetaData): Option[MetaData] = {
        val filtered = m.copy(year = 0, publishers = Buffer.empty, album = "", _type = "", _platform = "")
        if (filtered.authors.nonEmpty) Some(filtered) else None
      }
      trace(_ => s"Processing audio tag ${audioTag} with hashes: ${hashes}")
      val allHashes = hashes.toSet
      var remainingHashes = hashes
      val removedHashes = scala.collection.mutable.HashSet.empty[String]
      var remHashesSize = remainingHashes.size
       // use all hashes for final pass
      var metas = remainingHashes.flatMap(h => metasByHash.get(h).orElse(if (processAgain) Some(MetaData(h, Buffer.empty, Buffer.empty, "", 0, "", "")) else None))
      var metasSize = metas.size
      trace(_ => s"Found ${metasSize} matching metas for audio tag ${audioTag}: ${metas.seq}")
      var anyMetadata = metas.exists(m => m.authors.nonEmpty || m.publishers.nonEmpty || m.album.nonEmpty || m.year != 0)
      while (anyMetadata && metasSize > 1 && remHashesSize > 1) {
        val cmp = metas.head
        val skipMeta =
          (metas.forall(m => m.publishers == cmp.publishers && m.album == cmp.album && m.year == cmp.year) ||
           metas.forall(m => m.publishers.isEmpty && m.album.isEmpty && m.year <= 0)) &&
            (metas.forall(_.authors.nonEmpty) || metas.forall(_.authors.isEmpty))
        if (!skipMeta || remHashesSize != metasSize) {
          val cachedDups = audio.duplicatesForTag(audioTag)(cmp.hash, !processAgain).filter(h => processAgain || allHashes.contains(h))
          val duplicateHashes = cachedDups.iterator.filterNot(removedHashes.contains).toBuffer.sorted
          val sourceHashes = duplicateHashes.filterNot(h => !processAgain && excludedHashes.contains(h))
          val _duplicateMetas = duplicateHashes.flatMap(h => metasByHash.get(h)).flatMap(filterByConstraints)
          val _sourceMetas = sourceHashes.flatMap(h => metasByHash.get(h)).flatMap(filterByConstraints)
          def __filter(m: MetaData, metas: Buffer[MetaData]): Option[MetaData] = {
            if (!cachedDups.forall(h => _filter(m.copy(hash = h)))) {
              lazy val compatibleAuthors = metas.forall(m2 => areAuthorsCompatible(m.authors, m2.authors, knownAuthors))
              lazy val compatibleMetas = metas.forall(m2 => (m2.publishers.isEmpty || m.publishers.isEmpty || m.publishers.map(normalizePublisher).exists(m2.publishers.map(normalizePublisher).contains)) && (m.album.isEmpty || m2.album.isEmpty || normalizeAlbum(m) == normalizeAlbum(m2)) && (m.year == 0 || m2.year == 0 || m.year == m2.year))
              lazy val earlierYear = metas.exists(m2 => m.year > 0 && m2.year > 0 && m2.year < m.year)
              lazy val normAuthors = m.authors.map(normalizeAuthor).sorted.distinct
              lazy val authorMetas = authenticAuthorMetas.getOrElse(normAuthors, Set.empty).filter(m2 => m2.hash.nonEmpty && m2.authors.map(normalizeAuthor).sorted.distinct == normAuthors && m2.year == m.year)
              lazy val lonelyMeta = authorMetas.isEmpty || authorMetas.forall(m2 => m2.publishers == m.publishers && m2.album == m.album)
              var meta = keepAuthor(m)
              if (meta.isDefined && compatibleAuthors && ((compatibleMetas && lonelyMeta) || earlierYear)) {
                debug(s"Overriding meta data entry ${m} with ${meta.get} because does not pass constraint filter - metas: ${metas}, authorMetas: ${authorMetas} compatibleAuthors: ${compatibleAuthors}, compatibleMetas: ${compatibleMetas}, lonelyMeta: ${lonelyMeta}, earlierYear: ${earlierYear}")
                metasByHash(m.hash) = meta.get
              } else if (!meta.isDefined && compatibleMetas) {
                debug(s"Removing meta data entry ${m} because does not pass constraint filter and has no authors - metas: ${metas}")
                metasByHash.remove(m.hash)
              } else {
                debug(s"Keeping meta data entry ${m} - metas: ${metas}, compatibleAuthors: ${compatibleAuthors}, compatibleMetas: ${compatibleMetas}, authorMetas: ${authorMetas}, lonelyMeta: ${lonelyMeta}, earlierYear: ${earlierYear}")
                meta = Some(m)
              }
              meta
            } else Some(m)
          }
          val duplicateMetas = _duplicateMetas.flatMap(m => __filter(m, _duplicateMetas)).distinct
          val sourceMetas = _sourceMetas.flatMap(m => __filter(m, _sourceMetas)).distinct
          val h = sourceMetas.headOption.getOrElse(null)
          val skipMeta = sourceMetas.isEmpty || (duplicateHashes.size == duplicateMetas.size && (
            (sourceMetas.forall(m => m.publishers == h.publishers && m.album == h.album && m.year == h.year) ||
             sourceMetas.forall(m => m.publishers.isEmpty && m.album.isEmpty && m.year <= 0)) &&
             (sourceMetas.forall(_.authors.nonEmpty) || sourceMetas.forall(_.authors.isEmpty))))
          trace(_ => s"Metas or hashes differ for audio tag ${audioTag}, comparing ${cmp.hash} with ${remainingHashes.size} hashes and cached dups ${cachedDups.mkString(", ")}, skipMeta: ${skipMeta}, duplicateHashes: ${duplicateHashes.seq}, duplicateMetas: ${duplicateMetas.seq}")
          if (!skipMeta && duplicateHashes.size > 1 && sourceMetas.nonEmpty) {
            trace(_ => s"Found ${duplicateHashes.size} duplicate hashes for audio tag ${audioTag}: ${duplicateHashes.mkString(", ")} with metas: ${duplicateMetas.mkString(" | ")}")
            // select best based on some ad hoc metadata heuristics
            val rawMinyear = sourceMetas.map(e => if (e.year > 0) e.year else 9999).min
            val validSourceMetas = if (rawMinyear < 9999) sourceMetas.filter(e => e.year > 0 && (e.year <= rawMinyear + 1 || (e._type == "Compo" && e.year <= rawMinyear + 2))) else sourceMetas
            def score(e: MetaData): Int = {
              (if (e.authors.nonEmpty) 1 else 0) +
              (if (e.publishers.nonEmpty || e._type == "Tool") 10 else 0) +
              (if (e.album.nonEmpty || e._type == "Compo") 1000 else 0) +
              (if (e.year > 0) 10000 else 0) +
              (if (e.year <= rawMinyear) 100 else 0) +
              (if (e._platform.toLowerCase == "amiga" || e._type == "Compo") 1 else 0) +
              (if (e._type.toLowerCase == "game") 1 else 0)
            }
            val sourceScores = validSourceMetas.map(e => (e.hash, score(e))).toMap
            val bestscore = if (sourceScores.isEmpty) 0 else sourceScores.values.max
            val bestmetas = validSourceMetas.filter(e => sourceScores.getOrElse(e.hash, 0) == bestscore)
            val minyear =
              if (bestmetas.isEmpty) 9999
              else bestmetas.map(e => if (e.year > 0) e.year else 9999).min
            val maxauthors = if (bestmetas.isEmpty) 0 else bestmetas.filter(m => (m.year == 0 && minyear == 9999) || m.year <= minyear).map(_.authors.size).max
            val maxpublishers = if (bestmetas.isEmpty) 0 else bestmetas.filter(m => (m.year == 0 && minyear == 9999) || m.year <= minyear).map(_.publishers.size).max
            val byyear = bestmetas.filter(m => minyear == 9999 || (m.year > 0 && m.year <= minyear))
            val byauthor = byyear.filter(_.authors.size == maxauthors)
            val bypublishers = if (byauthor.isEmpty) byyear.filter(e => e.publishers.size == maxpublishers || maxpublishers == 0) else byauthor.filter(e => e.publishers.size == maxpublishers || maxpublishers == 0)
            var bests = if (bypublishers.nonEmpty) bypublishers else if (byauthor.nonEmpty) byauthor else byyear
            bests = bests
              .sortBy(b => (if (b._type.toLowerCase == "game") "AAAAA" else "ZZZZZ",
                           if (b.album.isEmpty) "ZZZZZ" else normalizeAlbum(b)))
            // pick majority publisher/album combination matching minyear
            def pickBySourcePriority(
              candidates: Iterable[MetaData]
            ): Option[MetaData] = {
              authorSources.iterator.map { source =>
                val matches = candidates.filter(c => source.get(c.hash).exists(src =>
                  areAuthorsCompatible(src.authors, c.authors, knownAuthors) &&
                  (src.publishers == c.publishers || src.publishers.map(normalizePublisher).exists(c.publishers.map(normalizePublisher).contains)) &&
                  normalizeAlbum(src) == normalizeAlbum(c) &&
                  src.year == c.year
                ))
                if (matches.nonEmpty) Some(matches.maxBy(_.authors.size))
                else None
              }.collectFirst { case Some(x) => x }
            }

            var best = if (bests.nonEmpty) {
              val grouped = {
                val grouped = bests.filter(m => m.publishers.nonEmpty && m.album.nonEmpty).groupBy(m => (m.publishers.map(normalizePublisher).sorted, normalizeAlbum(m)))
                if (grouped.isEmpty) {
                   bests.groupBy(m => (m.publishers.map(normalizePublisher).sorted, normalizeAlbum(m)))
                } else grouped
              }
              val maxCount = grouped.values.map(_.size).max
              val tiedGroups = grouped.filter(_._2.size == maxCount)
              val candidates = tiedGroups.values.flatten.toSeq.sortBy(_.hash)
              val best = if (candidates.size > 1) {
                pickBySourcePriority(candidates).getOrElse(candidates.maxBy(_.authors.size))
              } else {
                candidates.maxBy(_.authors.size)
              }
              trace(_ => s"Best candidate by year/publisher/album majority for ${audioTag} with min year ${minyear} is ${best.hash} with publishers ${best.publishers.mkString(", ")} and album ${best.album} and year ${best.year} candidates: ${candidates.mkString(" | ")} grouped size: ${grouped.size} bests: ${bests.mkString(" | ")} bestmetas: ${bestmetas.mkString(" | ")} bestscore: ${bestscore}") 
              best
            } else {
              // fallback: use majority voting on publisher/album for all bestmetas
              val grouped = {
                val grouped = bestmetas.filter(m => m.publishers.nonEmpty && m.album.nonEmpty).groupBy(m => (m.publishers.map(normalizePublisher).sorted, normalizeAlbum(m)))
                if (grouped.isEmpty) {
                   bestmetas.groupBy(m => (m.publishers.map(normalizePublisher).sorted, normalizeAlbum(m)))
                } else grouped
              }
              if (grouped.nonEmpty) {
                val maxCount = grouped.values.map(_.size).max
                val tiedGroups = grouped.filter(_._2.size == maxCount)
                val candidates = tiedGroups.values.flatten.toSeq.sortBy(_.hash)
                val best = if (candidates.size > 1) {
                  pickBySourcePriority(candidates).getOrElse(candidates.maxBy(_.authors.size))
                } else {
                  candidates.head
                }
                trace(_ => s"Best candidate by publisher/album majority for ${audioTag} is ${best.hash} with publishers ${best.publishers.mkString(", ")} and album ${best.album} and year ${best.year} candidates: ${candidates.mkString(" | ")} grouped size: ${grouped.size} bestmetas: ${bestmetas.mkString(" | ")} bestscore: ${bestscore}")
                best
              } else {
                // fallback: use majority voting on authors for all bestmetas
                val grouped = {
                  val grouped = bestmetas.filter(_.authors.nonEmpty).groupBy(m => m.authors.map(normalizeAuthor).sorted)
                  if (grouped.isEmpty) {
                     bestmetas.groupBy(m => m.authors.map(normalizeAuthor).sorted)
                  } else grouped
                }
                val best = if (grouped.nonEmpty) {
                  val maxCount = grouped.values.map(_.size).max
                  val tiedGroups = grouped.filter(_._2.size == maxCount)
                  val candidates = tiedGroups.values.flatten.toSeq.sortBy(_.hash)
                  if (candidates.size > 1) {
                    pickBySourcePriority(candidates).getOrElse(candidates.head)
                  } else {
                    candidates.head
                  }
                } else {
                  bestmetas.maxBy(_.authors.size)
                }
                trace(_ => s"Best candidate by author majority for ${audioTag} is ${best.hash} with authors ${best.authors.mkString(", ")} and publishers ${best.publishers.mkString(", ")} and album ${best.album} and year ${best.year} candidates: ${bestmetas.mkString(" | ")} grouped size: ${grouped.size} bestmetas: ${bestmetas.mkString(" | ")} bestscore: ${bestscore}")
                best
              }
            }
            // use the most common authors as best
            debug(s"Combining ${duplicateHashes.mkString(", ")} to ${best.hash} with score ${bestscore} (${duplicateMetas.map(e => s"${e}:${score(e)}").mkString(", ")}) duplicate metas: ${duplicateMetas.mkString(" | ")} best: ${best} bestscore: ${bestscore} bests: ${bests.mkString(" | ")} bestmetas: ${bestmetas.mkString(" | ")}")

            // build a subsong count map for all candidates using songlengths
            val subsongCountMap: Map[String, Int] = cachedDups.map { h =>
              val entries = songlengths.songlengthsByMd5(h)
              h -> entries.headOption.map(_.subsongs.size).getOrElse(-1)
            }.toMap
            // build an ordered candidate list: try the selected `best` first, then other candidates by score
            val candidatesGrouped = duplicateMetas.distinct.sortBy(m => (-score(m), m.hash))
              .groupBy(c => (
                subsongCountMap.getOrElse(c.hash, -1),
                c.authors,
                c.publishers,
                c.album,
                c.year,
                c._type,
                c._platform
              ))
            val candidatesSorted = candidatesGrouped
              .values
              .filter(v => v.forall(_filter))
              .map(_.head)
              .toSeq
              .distinct
              .sortBy(m => (-score(m), if (m.album.isEmpty) "ZZZZZ" else normalizeAlbum(m), m.hash))
            // compute per-candidate fallback authors once (avoid recomputing per-hash)
            lazy val candAuthorsMap: Map[String, Buffer[String]] = (Seq(best) ++ candidatesSorted).map { cand =>
              val authors = if (cand.authors.nonEmpty) cand.authors else {
                val authorGroups = duplicateMetas.groupBy(_.authors.sorted).filter(_._1.nonEmpty)
                if (authorGroups.nonEmpty) {
                  val maxCount = authorGroups.values.map(_.size).max
                  val tiedAuthors = authorGroups.filter(_._2.size == maxCount).keys.toSeq.sortBy(_.mkString(","))
                  authorSources.iterator.map { source =>
                    tiedAuthors.find { authors =>
                      duplicateHashes.exists(hash => source.get(hash).exists(s => areAuthorsCompatible(s.authors, authors, knownAuthors)))
                    }
                  }.collectFirst { case Some(x) => x }.getOrElse(tiedAuthors.head)
                } else Buffer.empty
              }
              cand.hash -> authors
            }.toMap

            for (hash <- duplicateHashes) {
              val meta = metasByHash.get(hash)
              lazy val normAuthors = meta.map(_.authors.flatMap(a => getAuthorVariants(a, knownAuthors)).map(normalizeAuthor)).getOrElse(Seq.empty)
              // Try candidates in order until one passes the compatibility checks
              var applied = false
              val hashScore = meta.map(score).getOrElse(0)
              val hashSubsongCount = subsongCountMap.getOrElse(hash, -1)
              val hashPlatform = if (meta.isDefined) meta.get._platform else ""
              val hashType = if (meta.isDefined) meta.get._type else ""
              val allowOverride = !meta.isDefined || (cachedDups.exists(h => !_filter(meta.get.copy(hash = h)) && subsongCountMap.getOrElse(h, -1) == hashSubsongCount) && candidatesSorted.forall(c => areAuthorsCompatible(meta.get.authors, c.authors, knownAuthors)))
              val hasYear = meta.exists(_.year > 0)
              val hasAuthors = meta.exists(_.authors.nonEmpty)
              // Reorder candidates for this hash: prioritize subsong match, platform match, type match, then score
              def formatScore(m: MetaData): Int = {
                val e = songlengths.songlengthsByMd5(m.hash).head
                if (e.format.toLowerCase.contains("soundtracker")) 2
                else if (e.format.toLowerCase.contains("noisetracker")) 1
                else 0
              }
              def constraintYear(m: MetaData): Int = if (m.year > 0) m.year else md5Constraints.getOrElse(m.hash, (9999, "", "", Set.empty))._1
              val _cands = (Seq(best) ++ candidatesSorted)
              val yearMissing = _cands.forall(_.year == 0)
              var candsByPriority = _cands
                .flatMap { cand =>
                  if (!_filter(cand.copy(hash = hash)) || !cachedDups.forall(h => _filter(cand.copy(hash = h)))) {
                    val _cand = cand.copy(publishers = Buffer.empty, album = "", year = 0, _type = "", _platform = "")
                    if (_cand.authors.nonEmpty) Some(cand) else None
                  } else Some(cand)
                }
                .filter(c => (allowOverride && ((!hasYear || c.year > 0) || (!hasAuthors && c.authors.nonEmpty))) || score(c) >= hashScore)
                .distinctBy(c => (subsongCountMap.getOrElse(c.hash, -1), c.authors, c.publishers, c.album, c.year, c._type, c._platform))
                .sortBy { cand =>
                  val candSubsongCount = subsongCountMap.getOrElse(cand.hash, -1)
                  val subsongMatch = if (hashSubsongCount > 0 && candSubsongCount > 0) if (hashSubsongCount == candSubsongCount) -1 else 0 else 0
                  val platformMatch = if (hashPlatform.nonEmpty && cand._platform.nonEmpty) if (hashPlatform.toLowerCase == cand._platform.toLowerCase) -1 else 0 else 0
                  val typeMatch = if (hashType.nonEmpty && cand._type.nonEmpty) (if (normalizeType(hashType) == normalizeType(cand._type)) -1 else 0) else 0
                  val gamePreference = if (cand._type.toLowerCase == "game") -1 else 0
                  val sourcePrio = authorSources.indexWhere(_.contains(cand.hash))
                  val sourceCount = sourceCounts.getOrElse(cand.hash, 0)
                  val year = if (yearMissing) constraintYear(cand) else if (cand.year > 0) cand.year else 9999
                  ((subsongMatch, platformMatch, typeMatch), if (cand.copy(hash = "") == best.copy(hash = "")) -1 else 0, (-score(cand), -formatScore(cand)), (gamePreference, sourcePrio, -sourceCount), (year, if (cand.album.isEmpty) "ZZZZZ" else normalizeAlbum(cand), cand.hash))
                }
                .distinct
              trace(_ => s"Candidates for hash ${hash} meta: ${meta} by priority: ${candsByPriority} candsSorted: ${candidatesSorted} allowOverride: ${allowOverride}")
              candsByPriority.foreach { cand =>
                val _allowOverride = allowOverride && ((!hasYear || cand.year > 0) || (!hasAuthors && cand.authors.nonEmpty))
                if (!applied) {
                  lazy val candAuthors = candAuthorsMap.getOrElse(cand.hash, Buffer.empty)
                  lazy val candNormAuthors = candAuthors.flatMap(a => getAuthorVariants(a, knownAuthors)).map(normalizeAuthor)
                  lazy val candNormPublishers = cand.publishers.map(normalizePublisher)
                  lazy val candNormAlbum = normalizeAlbum(cand)
                  lazy val candOk = (_allowOverride || meta.get.authors.isEmpty ||
                    normAuthors.exists(candNormAuthors.contains) ||
                    {
                      val metas = meta.get.authors.map(normalizeAuthor).flatMap(a => authorMetas.get(a)).flatten
                      if (metas.nonEmpty) {
                        val knownYears = metas.map(_.year).filter(_ > 0)
                        (cand.year == 0 || (knownYears.nonEmpty && cand.year >= knownYears.min - 1 && cand.year <= knownYears.max + 1)) &&
                        (candNormAlbum.isEmpty || metas.exists(m => normalizeAlbum(m) == candNormAlbum)) &&
                        (candNormPublishers.isEmpty || metas.exists(_.publishers.map(normalizePublisher).exists(candNormPublishers.contains)))
                      } else false
                    }) && (_allowOverride || meta.get._type.isEmpty || cand._type.nonEmpty)
                       && (_allowOverride || meta.get.year == 0 || (cand.year <= meta.get.year + 1 || (cand._type == "Compo" && cand.year <= meta.get.year + 2)))
                       && (_allowOverride || meta.get.year == 0 || meta.get._type.toLowerCase != "game" || cand._type.toLowerCase == "game" || cand.year + 1 < meta.get.year)

                  lazy val candScore = score(cand)
                  lazy val isCandPreview = isPreview(cand.album)

                  if ((_allowOverride || hashScore < candScore ||
                    (hashScore == candScore && cand.year > 0 && cand.year < meta.get.year) ||
                    (hashScore == candScore && !isCandPreview && isPreview(meta.get.album) && candNormAlbum == normalizeAlbum(meta.get)))
                    && (candOk || _allowOverride)) {
                    val old = meta.getOrElse(cand)
                    lazy val typeChange = (old._type.toLowerCase == "game" && cand._type.toLowerCase != "game") || (old._type.toLowerCase != "game" && cand._type.toLowerCase == "game")
                    lazy val differentNames = old.authors.map(normalizeAuthor).intersect(candAuthors.map(normalizeAuthor)).isEmpty
                    lazy val compatibleAuthors = areAuthorsCompatible(old.authors, candAuthors, knownAuthors)
                    lazy val authors =
                      if (old.authors.isEmpty && candAuthors.nonEmpty) candAuthors
                      else if (candAuthors.isEmpty && old.authors.nonEmpty) old.authors
                      else if ((old.authors.size < candAuthors.size && (candNormAuthors.exists(normAuthors.contains))) || (typeChange && candAuthors.size >= old.authors.size)) {
                        if (cand._type.toLowerCase == "game" && compatibleAuthors && differentNames && old.authors.forall(isRealName) && !cand.authors.forall(isRealName)) old.authors
                        else if (cand._type.nonEmpty && cand._type.toLowerCase != "game" && compatibleAuthors && differentNames && old.authors.forall(!isRealName(_)) && !candAuthors.forall(!isRealName(_))) old.authors
                        else if (compatibleAuthors && !differentNames && old.authors.size >= cand.authors.size) old.authors
                        else candAuthors
                      } else if (typeChange && compatibleAuthors && differentNames && (candAuthors.size >= old.authors.size || old.authors.exists(isRealName) != candAuthors.exists(isRealName))) candAuthors
                      else old.authors
                    val passesFilter = cachedDups.forall(h => _filter(cand.copy(hash = h)))
                    if (passesFilter && (!meta.isDefined || processAgain || compatibleAuthors || meta.get.authors.isEmpty)) {
                      val different = meta.isEmpty || meta.get.copy(hash = "") != cand.copy(hash = "")
                      val authorsUpdated = meta.isEmpty || authors != meta.get.authors
                      if (different || authorsUpdated) {
                        if (meta.isDefined) {
                          debug(s"Overriding meta data entry ${meta.get} with ${cand}, score ${score(meta.get)} candscore ${candScore} typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${_allowOverride}")
                          if (authors != old.authors) {
                            debug(s"Overriding authors for ${old} with ${authors}, cand: ${cand}, meta: ${meta}  typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${_allowOverride}")
                          }
                        } else {
                          debug(s"Overriding meta data for md5 ${hash} with ${cand}" + s" typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${_allowOverride}")
                        }
                        metasByHash(hash) = cand.copy(authors = authors, hash = hash)
                        applied = true
                      }
                    } else {
                      // candidate failed the main passesFilter check; try author-only override when it's safe
                      if (meta.isDefined && meta.get.authors.isEmpty && authors.nonEmpty) {
                        debug(s"Overriding authors for ${old} with ${authors}, cand: ${cand}, meta: ${meta}  typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${_allowOverride} passesFilter: ${passesFilter}")
                        metasByHash(hash) = meta.get.copy(authors = authors)
                        applied = true
                      } else if (!meta.isDefined && authors.nonEmpty) {
                        debug(s"Overriding authors for md5 ${hash} with ${authors}, cand: ${cand}, meta: ${meta}  typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${_allowOverride}" + s" passesFilter: ${passesFilter}")
                        metasByHash(hash) = MetaData(hash, authors, Buffer.empty, "", 0, "", "")
                        applied = true
                      } else {
                        trace(_ => s"Not overriding meta data for hash ${hash} meta ${meta} with ${cand}, score ${score(meta.get)} candscore ${candScore} candOk: ${candOk}, typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${_allowOverride} passesFilter: ${passesFilter}")
                      }
                    }
                  } else {
                    // candidate-specific author-only override
                    if (meta.isDefined) {
                      if ((meta.get.authors.isEmpty && candAuthors.nonEmpty) || (
                          meta.get.authors.size < candAuthors.size &&
                          candNormAuthors.exists(normAuthors.contains) &&
                          ((candNormPublishers.isEmpty && meta.get.publishers.isEmpty) || candNormPublishers.exists(meta.get.publishers.map(normalizePublisher).contains)) &&
                          ((cand.year == 0 && meta.get.year == 0) || cand.year == meta.get.year) &&
                          ((cand.album.isEmpty && meta.get.album.isEmpty) ||
                          (candNormAlbum == normalizeAlbum(meta.get))))
                      ) {
                        if (meta.get.authors != candAuthors.sorted) {
                          debug(s"Overriding authors for ${meta.get} with ${candAuthors.sorted} candOk: ${candOk} allowOverride: ${_allowOverride}")
                          metasByHash(hash) = meta.get.copy(authors = candAuthors.sorted)
                          applied = true
                        }
                      } else {
                        trace(_ => s"Not overriding meta data for ${meta.get} with ${cand}, score ${score(meta.get)} candscore ${candScore} year ${meta.get.year} cand year ${cand.year} candOk: ${candOk} allowOverride: ${_allowOverride}")
                      }
                    } else {
                      trace(_ => s"Not overriding meta data for md5 ${hash} with ${cand}, score ${score(meta.get)} candscore ${candScore} year  cand year ${cand.year} candOk: ${candOk} allowOverride: ${_allowOverride}")
                    }
                  }
                }
              }
            }
          } else if (duplicateHashes.size > 1) {
            trace(_ => s"SKIPPED1 processing audio tag ${audioTag} with duplicate metas ${duplicateMetas.seq} and hashes ${duplicateHashes.mkString(", ")}")
          }
        } else {
          trace(_ => s"SKIPPED2 processing audio tag ${audioTag} with metas ${metas.seq} and hashes ${remainingHashes.mkString(", ")} because metas or hashes are the same")
        }
        val (before, after) = remainingHashes.span(_ != cmp.hash)
        remainingHashes = before ::: after.drop(1)
        removedHashes += cmp.hash
        remHashesSize -= 1
        metas = metas.tail
        metasSize -= 1
        anyMetadata = metas.exists(m => m.authors.nonEmpty || m.publishers.nonEmpty || m.album.nonEmpty || m.year != 0)
      }
    }

    // process connected components in parallel; audioTags within each component run sequentially
    audio.components.par.foreach { component =>
      component.foreach { case (audioTag, componentHashes) =>
        if (processAgain && componentHashes.size > 1) {
          processAudioTag(audioTag, componentHashes.map(_._1))
        } else if (componentHashes.size > 1) {
          val grouped = collection.immutable.SortedMap.empty ++ componentHashes.groupBy(_._2)
          grouped.toSeq.reverse.foreach { case (subsongCount, hashes) =>
            processAudioTag(audioTag, hashes.map(_._1))
          }
        } else {
          trace(_ => s"SKIPPED processing audio tag ${audioTag} with hashes ${componentHashes.map(_._1).mkString(", ")} because only one hash")
        }
      }
    }
  }

  var finalMetas = metasByHash.values.seq.toBuffer

  for (pass <- 1 to 2) {

    val allmetas = (finalMetas ++ extraMetas)
      .par
      .filterNot(_.album.isEmpty)
      .seq

    val metasWithAlbum = allmetas
      .par
      .filterNot(_.album.isEmpty)
      .groupBy(m => normalizeAlbum(m))
      .mapValues(_.seq.toBuffer)
      .seq

    val yearPublisher = allmetas
      .par
      .filterNot(_.year == 0)
      .filterNot(_.publishers.isEmpty)
      .flatMap(m => {
        m.publishers.map(normalizePublisher).flatMap { publisher =>
          Seq(
            (m.year - 1, publisher),
            (m.year, publisher),
            (m.year + 1, publisher)
          )
        }
      })
      .seq.toSet

    val authorMetas = allmetas
      .par
      .filterNot(_.authors.isEmpty)
      .filterNot(m => m.year == 0 && m.publishers.isEmpty && m.album.isEmpty && m._type.isEmpty && m._platform.isEmpty)
      .flatMap(m => {
        m.authors.map(normalizeAuthor).flatMap { author =>
          Set(
            (author, m)
          )
        }
      })
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)
      .seq

    val albumPublishersYearTypePlatformToAuthors = allmetas
      .par
      .filterNot(m => m.authors.isEmpty || m.album.isEmpty || m.publishers.isEmpty || m.year == 0 || m._type.isEmpty || m._platform.isEmpty)
      .groupBy(m => (normalizeAlbum(m), m.publishers.map(normalizePublisher).sorted.distinct, m.year, normalizeType(m._type), m._platform.toLowerCase))
      .mapValues(_.map(_.authors).distinct)
      .seq

    finalMetas = finalMetas.par.map(m =>
      var meta = m
      boundary {
        if (meta.album.isEmpty || (meta.publishers.nonEmpty && meta.year > 0)) {
          break()
        }
        val key = normalizeAlbum(meta)
        var availableMetas = metasWithAlbum(key)
          .filterNot(_.hash == meta.hash)
          .filter(m => meta._platform.isEmpty || m._platform.isEmpty || m._platform.toLowerCase == meta._platform.toLowerCase)

        if (availableMetas.isEmpty || availableMetas.forall(m => m.publishers.isEmpty || m.year == 0)) {
          availableMetas = metasWithAlbum(key)
            .filterNot(_.hash == meta.hash)
        }
    
        var metas = availableMetas
          .filterNot(_.publishers.isEmpty)
          //.filterNot(_.year == 0)
          .filter(m => meta.year == 0 || Math.abs(m.year - meta.year) <= 1)
          .filter(m => (m._type.toLowerCase.startsWith("game") && meta._type.toLowerCase.startsWith("game")) || (!m._type.toLowerCase.startsWith("game") && !meta._type.toLowerCase.startsWith("game")))

        if (metas.isEmpty) {
          break()
        }

        if (metas.filter(m => filterByConstraints(m.copy(authors = Buffer.empty, hash = meta.hash)).isDefined).size >= 1) {
          metas = metas.filter(m => filterByConstraints(m.copy(authors = Buffer.empty, hash = meta.hash)).isDefined)
        }

        if (meta.year != 0 && metas.filter(m => m.year != 0 && m.year == meta.year).size >= 1) {
          metas = metas.filter(m => m.year != 0 && m.year == meta.year)
        } else if (meta.year == 0 && metas.filter(_.year != 0).size >= 1) {
          metas = metas.filter(_.year != 0)
        }

        if (metas.filter(_._platform.toLowerCase == meta._platform.toLowerCase).size >= 1) {
          metas = metas.filter(_._platform.toLowerCase == meta._platform.toLowerCase)
        }

        if (metas.filter(m => normalizeType(m._type) == normalizeType(meta._type)).size >= 1) {
          metas = metas.filter(m => normalizeType(m._type) == normalizeType(meta._type))
        }

        val slentry = songlengths.songlengthsByMd5(meta.hash).head

        if (slentry.player == "uade" && metas.exists(_._platform == "Amiga") && metas.exists(m => m._platform.nonEmpty && m._platform != "Amiga")) {
          metas = metas.filter(m => m._platform.isEmpty || m._platform == "Amiga")
        }

        if (slentry.format.toLowerCase.contains("tracker")) {
          metas = metas.filter(m => m.year == 0 || m.year >= 1987)
        }

        lazy val _normPublishers = meta.publishers.map(normalizePublisher)
        if (meta.publishers.nonEmpty && metas.filter(m => m.publishers.map(normalizePublisher).exists(_normPublishers.contains) || _normPublishers.exists(m.publishers.map(normalizePublisher).contains)).size >= 1) {
          metas = metas.filter(m => m.publishers.map(normalizePublisher).exists(_normPublishers.contains) || _normPublishers.exists(p => m.publishers.map(normalizePublisher).contains(p)))
        }

        if (metas.filter(m => _normalizeAlbum(m.album) == _normalizeAlbum(meta.album)).size >= 1) {
          metas = metas.filter(m => _normalizeAlbum(m.album) == _normalizeAlbum(meta.album))
        }


        val authorFiltered = metas.filter(m => (meta.authors.isEmpty || m.authors.nonEmpty || availableMetas.forall(m => m.authors.isEmpty || areAuthorsCompatible(meta.authors, m.authors, knownAuthors))) && areAuthorsCompatible(meta.authors, m.authors, knownAuthors))
        if (authorFiltered.size >= 1) {
          metas = authorFiltered
        } else if (!metas.forall(m => areAuthorsCompatible(meta.authors, m.authors, knownAuthors))) {
          metas = metas.filter(m => areAuthorsCompatible(meta.authors, m.authors, knownAuthors))
        }

        if (metas.isEmpty) {
          break()
        }

        lazy val metasPublishers = metas.filterNot(_.publishers.isEmpty).map(_.publishers.map(normalizePublisher).distinct.toSet).toSet
        lazy val publisherComponents = {
          val pending = metasPublishers.toBuffer
          val components = scala.collection.mutable.Buffer.empty[scala.collection.mutable.Buffer[Set[String]]]

          while (pending.nonEmpty) {
            val component = scala.collection.mutable.Buffer.empty[Set[String]]
            val queue = scala.collection.mutable.Queue[Set[String]](pending.remove(0))

            while (queue.nonEmpty) {
              val group = queue.dequeue()
              component += group

              val (matches, rest) = pending.partition(other => other.exists(group.contains))
              pending.clear()
              pending ++= rest
              queue ++= matches
            }

            components += component
          }

          components
        }
        if (metas.forall(m =>
          (meta._platform.nonEmpty && m._platform.nonEmpty && m._platform != meta._platform) ||
          (meta._type.nonEmpty && m._type.nonEmpty && normalizeType(m._type) != normalizeType(meta._type)) ||
          (meta.year != 0 && m.year != 0 && Math.abs(m.year - meta.year) > 1) ||
          (_normPublishers.nonEmpty && m.publishers.nonEmpty && !m.publishers.map(normalizePublisher).exists(_normPublishers.contains) && !_normPublishers.exists(p => m.publishers.map(normalizePublisher).contains(p))) ||
          (meta.authors.nonEmpty && m.authors.nonEmpty && !areAuthorsCompatible(meta.authors, m.authors, knownAuthors))) || (((key.length <= 5 || Set("musicdisk","megademo","slideshow").exists(key.startsWith)) && publisherComponents.size > 1))
        ) {
          break()
        }

        lazy val _metas = metas
          .sortBy(m => (if (m.year > 0) m.year else 9999, -m.publishers.size))

        lazy val cmp = _metas.filter(m => _normalizeAlbum(m.album) == _normalizeAlbum(meta.album)).headOption.getOrElse(_metas.head)
        lazy val normPublishers = cmp.publishers.map(normalizePublisher).sorted.distinct

        lazy val yearOk = _metas.forall(m => m.year == 0 || Math.abs(m.year - cmp.year) <= 1) ||
          _metas.filter(_.hash.nonEmpty).forall(m => m.year == 0 || Math.abs(m.year - cmp.year) <= 1)
        lazy val publishersOk = yearOk ||
          _metas.forall(m => m.publishers.isEmpty || normPublishers.exists(p => m.publishers.map(normalizePublisher).contains(p))) ||
          _metas.filterNot(_ == cmp).exists(m => m.publishers.map(normalizePublisher).exists(normPublishers.contains))

        lazy val yearPlatformTypeMatch = _metas
          .filter(m => meta._platform.isEmpty || m._platform.isEmpty || m._platform == meta._platform)
          .filter(m => meta._type.isEmpty || m._type.isEmpty || normalizeType(m._type) == normalizeType(meta._type))
          .forall(m => m.year == 0 || Math.abs(m.year - cmp.year) <= 1)

        lazy val cmpOk = filterByConstraints(cmp.copy(authors = Buffer.empty, hash = meta.hash)).isDefined

        if (meta.publishers.isEmpty && meta.year == 0 && cmpOk) {
          val authorMatches = meta.authors.map(normalizeAuthor).flatMap(a => authorMetas.get(a)).flatten
            .filter(e =>
              meta._type.isEmpty || (e._type.toLowerCase.startsWith("game") && meta._type.toLowerCase.startsWith("game")) ||
              (!e._type.toLowerCase.startsWith("game") && !meta._type.toLowerCase.startsWith("game")))
            .filter(e => meta._platform.isEmpty || e._platform.toLowerCase == meta._platform.toLowerCase)
            .distinct

          if (!(meta.authors.isEmpty || authorMatches.nonEmpty)) {
            break()
          }

          if (publishersOk && yearPlatformTypeMatch &&
              normPublishers.exists(p => yearPublisher.contains((cmp.year, p)))
          ) {
            debug(s"Filling publishers and year for ${meta} - publishers ${meta.publishers.mkString(",")} -> ${cmp.publishers.mkString(",")}, year ${meta.year} -> ${cmp.year} source: ${cmp}")
            meta = meta.copy(publishers = cmp.publishers, year = cmp.year)
          } else if (publishersOk && yearPlatformTypeMatch && cmp.year == 0 && meta.year == 0 && cmp._platform == meta._platform) {
            debug(s"Filling publishers for ${meta} -  publishers ${meta.publishers.mkString(",")} -> ${cmp.publishers.mkString(",")} source: ${cmp}")
            meta = meta.copy(publishers = cmp.publishers)
          } else if (yearPlatformTypeMatch && yearOk) {
            debug(s"Filling year for ${meta} -  year ${meta.year} -> ${cmp.year} source: ${cmp}")
            meta = meta.copy(year = cmp.year)
          }

        } else if (meta.publishers.isEmpty && meta.year != 0 && yearPlatformTypeMatch && cmpOk) { 
          if (publishersOk && yearOk
          ) {
            debug(s"Filling publishers for ${meta} -  publishers ${meta.publishers.mkString(",")} -> ${cmp.publishers.mkString(",")} source: ${cmp}")
            meta = meta.copy(publishers = cmp.publishers)
          }

        } else if (meta.publishers.nonEmpty && meta.year == 0 && cmp.year != 0 && yearPlatformTypeMatch && cmpOk) {
          if (yearOk && publishersOk
          ) {
            debug(s"Filling year for ${meta} -  year ${meta.year} -> ${cmp.year} source: ${cmp}")
            meta = meta.copy(year = cmp.year)
          }
        } else if (!cmpOk) {
          debug("Removing metadata due to cmp mismatch for key: " + key + " META: " + meta + " CMP: " + cmp + " METAS: " + _metas.mkString(" | ") + " cmpOk: " + cmpOk)
          meta = meta.copy(album = "", publishers = Buffer.empty, year = 0, _type = "", _platform = "")
        }
      }
      meta
    ).par.map(m =>
      var meta = m
      // fill missing authors based on unique authors + album + publishers + year combination
      boundary {
        if (meta.album.isEmpty || meta.authors.nonEmpty || meta.publishers.isEmpty || meta.year == 0) {
          break()
        }
        val key = normalizeAlbum(meta)
        var availableMetas = metasWithAlbum(key)
          .filterNot(_.hash == meta.hash)
          .filter(m => m.authors.nonEmpty && (meta._platform.isEmpty || m._platform.isEmpty || m._platform.toLowerCase == meta._platform.toLowerCase))

        if (availableMetas.isEmpty || availableMetas.forall(m => m.authors.isEmpty || m.publishers.isEmpty || m.year == 0)) {
          availableMetas = metasWithAlbum(key)
            .filterNot(_.hash == meta.hash)
        }

        val normPublishers = meta.publishers.map(normalizePublisher).sorted.distinct
        var metas = availableMetas
          .filter(m => (m._type.toLowerCase.startsWith("game") && meta._type.toLowerCase.startsWith("game")) || (!m._type.toLowerCase.startsWith("game") && !meta._type.toLowerCase.startsWith("game")))
          .filter(m => m.publishers.map(normalizePublisher).exists(normPublishers.contains) || normPublishers.exists(p => m.publishers.map(normalizePublisher).contains(p)))
          .filter(m => m.year == meta.year)

        if (metas.isEmpty) {
          break()
        }

        if (metas.filter(m => m.authors.nonEmpty && m._platform.toLowerCase == meta._platform.toLowerCase).size >= 1) {
          metas = metas.filter(_._platform.toLowerCase == meta._platform.toLowerCase)
        }

        if (metas.filter(m => normalizeType(m._type) == normalizeType(meta._type)).size >= 1) {
          metas = metas.filter(m => normalizeType(m._type) == normalizeType(meta._type))
        }
  
        if (metas.filter(m => m.authors.nonEmpty && _normalizeAlbum(m.album) == _normalizeAlbum(meta.album)).size >= 1) {
          metas = metas.filter(m => _normalizeAlbum(m.album) == _normalizeAlbum(meta.album))
        }

        metas = metas
          .filterNot(_.authors.isEmpty)
          .filterNot(_.publishers.isEmpty)
          .filterNot(_.year == 0)
      
        if (metas.isEmpty) {
          break()
        }

        lazy val normAlbum = normalizeAlbum(meta)
        val metas_ = metas.filter(_.hash.nonEmpty)
        if (metas_.size > 1 && (normAlbum.contains("megademo") || meta._type.toLowerCase == "musicdisk" || metas_.exists(_._type.toLowerCase == "musicdisk") || meta._type.toLowerCase.contains("pack") || meta._type.toLowerCase.contains("tool") ||
        !metas_.forall(m => areAuthorsCompatible(m.authors, metas_.head.authors, knownAuthors)))) {
          val audioHashes = metas_.flatMap(m => audio.audioHashesByMd5.getOrElse(m.hash, Buffer.empty)).toSet
          if (audioHashes.size > 1) {
            break()
          }
        }

        var authenticCMPs = metas.filter(m => authenticAuthorMetas.contains(m.authors.map(normalizeAuthor).sorted.distinct))
        if (m._type.toLowerCase == "game") {
          val withrealnames = authenticCMPs.filter(m => hasRealNames(m.authors))
          if (withrealnames.nonEmpty) {
            authenticCMPs = withrealnames
          }
        }
        val authenticCMPsWithAlbum = authenticCMPs.filter(m => authenticAuthorMetas(m.authors.map(normalizeAuthor).sorted.distinct).filter(a => normalizeAlbum(a) == normAlbum).nonEmpty)
        val authenticCMPsWithPublisher = authenticCMPs.filter(m => authenticAuthorMetas(m.authors.map(normalizeAuthor).sorted.distinct).filter(a => a.publishers.map(normalizePublisher).exists(meta.publishers.map(normalizePublisher).contains)).nonEmpty)

        val cmp =
          authenticCMPsWithAlbum.filter(_.hash.nonEmpty).headOption
          .orElse(authenticCMPsWithAlbum.headOption)
          .orElse(authenticCMPsWithPublisher.filter(_.hash.nonEmpty).headOption)
          .orElse(authenticCMPsWithPublisher.headOption)

        if (!cmp.isDefined) {
          break()
        }
          /*
          .orElse(authenticCMPsWithYear.filter(_.hash.nonEmpty).headOption)
          .orElse(authenticCMPsWithYear.headOption)
          .orElse(authenticCMPs.filter(_.hash.nonEmpty).headOption)
          .orElse(authenticCMPs.headOption)
          .orElse(metas.filter(_.hash.nonEmpty).filter(a => normalizeAlbum(a) == normAlbum).headOption)
          .orElse(metas.filter(a => normalizeAlbum(a) == normAlbum).headOption)
          .orElse(metas.filter(_.hash.nonEmpty).headOption)
          .getOrElse(metas.head)
          */
        if (!haveCompatibleAuthors(metas.map(_.authors) :+ cmp.get.authors, knownAuthors)) {
          break()
        }
        val _key = (normalizeAlbum(meta), cmp.get.publishers.map(normalizePublisher).sorted.distinct, cmp.get.year, normalizeType(cmp.get._type), cmp.get._platform.toLowerCase)
        val authorsSets = albumPublishersYearTypePlatformToAuthors.get(_key).getOrElse(Buffer.empty)
        if (authorsSets.nonEmpty && authorsSets.size > 1 && !haveCompatibleAuthors(authorsSets.toBuffer, knownAuthors)) {
          break()
        }
        // TODO md5 + audio hash count (Smells Like Amiga Spirit)
        debug(s"Filling authors for key: ${key}, meta: ${meta}, source: ${cmp}, authenticCMPs: ${authenticCMPs.mkString(" | ")}, authenticCMPsWithAlbum: ${authenticCMPsWithAlbum.mkString(" | ")}, authenticCMPsWithPublisher: ${authenticCMPsWithPublisher.mkString(" | ")}, metas: ${metas.mkString(" | ")}")
        meta = meta.copy(authors = cmp.get.authors)
      }

      if (meta.album.nonEmpty && meta.publishers.nonEmpty && meta._type.isEmpty) {
        val metas = metasWithAlbum(normalizeAlbum(meta))
          .filter(_.publishers.map(normalizePublisher).exists(meta.publishers.map(normalizePublisher).contains))
        val _type = metas.filterNot(_._type.isEmpty).headOption.map(_._type).getOrElse("")
        if (_type.nonEmpty && metas.forall(m => m._type.isEmpty || normalizeType(m._type) == normalizeType(_type))) {
          debug(s"Filling type for ${meta} - type: ${meta._type} -> ${_type}, source: ${metas.mkString(" | ")}")
          meta = meta.copy(_type = _type)
        }
      }

      if (meta.album.nonEmpty && meta.publishers.nonEmpty && meta._platform.isEmpty) {
        val metas = metasWithAlbum(normalizeAlbum(meta))
          .filter(_.publishers.map(normalizePublisher).exists(meta.publishers.map(normalizePublisher).contains))
        val _platform = metas.filterNot(_._platform.isEmpty).headOption.map(_._platform).getOrElse("")
        if (_platform.nonEmpty && metas.forall(m => m._platform.isEmpty || m._platform == _platform)) {
          debug(s"Filling platform for ${meta} - platform: ${meta._platform} -> ${_platform}, source: ${metas.mkString(" | ")}")
          meta = meta.copy(_platform = _platform)
        }
      }
      meta
    ).toBuffer.sortBy(_.hash).distinct
  }

  finalMetas.par.map(m => {
    val lcalbum = m.album.toLowerCase
    var updated = m
    if ((isCracktro(m._type)) &&
       m.album.nonEmpty &&
       !m.album.matches(".* \\+[0-9]+$") &&
       !m.album.matches(".* \\(\\+[0-9]+\\)$") &&
       !m.album.matches(".* [0-9]+%$") &&
       !lcalbum.contains(" 100% ") &&
       !lcalbum.contains(" keygen ") &&
       !lcalbum.endsWith(" ++") &&
       !lcalbum.endsWith(" intro") &&
       !lcalbum.endsWith(" trainer") &&
       !lcalbum.endsWith("-trainer") &&
       !lcalbum.endsWith(" trainer menu") &&
       !lcalbum.endsWith(" import") &&
       !lcalbum.endsWith(" pal/ntsc selector") &&
       !lcalbum.endsWith(" cd-rip") &&
       !lcalbum.endsWith("aga fix") &&
       !lcalbum.endsWith("hd fix") &&
       !lcalbum.endsWith("hd install") &&
       !lcalbum.endsWith(" pal fixed") &&
       !lcalbum.endsWith(" onefiled") &&
       !lcalbum.endsWith(" one filed") &&
       !lcalbum.endsWith(" onedisked") &&
       !lcalbum.endsWith(" one disked") &&
       !lcalbum.endsWith(" w/ save") &&
       !lcalbum.contains(" loader (") &&
       !lcalbum.endsWith(" crack") &&
       !lcalbum.endsWith(" cracktro") &&
       !lcalbum.startsWith("cracktro ") &&
       !lcalbum.contains(" cracktro ")
    ) {
      updated = m.copy(album = m.album + " [cracktro]")
    } 

    if (isPreview(lcalbum)) {
      lazy val normAlbum = normalizeAlbum(m)
      lazy val normPublishers = m.publishers.map(normalizePublisher)
      var nonpreviewmetas = authenticAuthorMetas.getOrElse(m.authors.map(normalizeAuthor).sorted.distinct, Set.empty).filter(m2 => m2.album != m.album && normAlbum == normalizeAlbum(m2) && !isPreview(m2.album.toLowerCase) && (m.year == 0 || m2.year == 0 || m2.year <= m.year) && (normPublishers.isEmpty || m2.publishers.isEmpty || m2.publishers.map(normalizePublisher).exists(normPublishers.contains)))
      if (nonpreviewmetas.filter(_.hash.nonEmpty).size >= 1) {
        nonpreviewmetas = nonpreviewmetas.filter(_.hash.nonEmpty)
      }
      val nonpreviewmeta = nonpreviewmetas.headOption
      lazy val audioHashes1 = audio.audioHashesByMd5.getOrElse(m.hash, Iterable.empty).toSet
      lazy val audioHashes2 = nonpreviewmetas.flatMap(m => audio.audioHashesByMd5.getOrElse(m.hash, Set.empty))
      if (nonpreviewmeta.isDefined && (audioHashes2.isEmpty || audioHashes2.exists(audioHashes1.contains))) {
        updated = m.copy(album = nonpreviewmeta.get.album)
        debug(s"Stripped preview/demo from album name for ${m.hash} - ${m.album} -> ${updated.album}, non-preview meta: ${nonpreviewmeta.get}")
      // XXX
      } else if (m.album == "World Of Commodore 92 Preview") {
        updated = m.copy(album = "World of Commodore")
      }
    }
    if (m._type.toLowerCase == "game" && m.authors.exists(!isRealName(_)) && (m.publishers.isEmpty || !m.publishers.forall(sceneGroups.contains))) {
      var realNameAuthors = updated.authors.map(a => if (isRealName(a)) a else getAuthorVariants(a, knownAuthors).sortBy(_.length).find(isRealName).getOrElse(a)).sorted.distinct
      if (realNameAuthors != updated.authors) {
        // XXX "Rod Thacker" vs "Jochen Hippel" vs "Mad Max"
        if (realNameAuthors.contains("Rod Thacker") && updated.authors.contains("Mad Max")) {
          realNameAuthors = realNameAuthors.map(_.replace("Rod Thacker", "Jochen Hippel"))
        }
        debug(s"Replacing non-real name authors with real names for ${updated} -> ${realNameAuthors.mkString(", ")}")
        updated = updated.copy(authors = realNameAuthors)
      }
    }
    val authors = updated.authors.map(a => unnormalizedAuthors.getOrElse(a, a)).sorted.distinct
    if (authors != updated.authors) {
      debug(s"Unnormalizing authors for ${updated} -> ${authors.mkString(", ")}")
      updated = updated.copy(authors = authors)
    }

    if (m._type.nonEmpty && m._type.toLowerCase != "game" && updated.authors.exists(isRealName) && !updated.publishers.exists(nonSceneGroups.contains)) {
      var authors = updated.authors
      for (author <- updated.authors if isRealName(author)) {
        val aliases = getAuthorVariants(author, knownAuthors).filter(!isRealName(_))
        boundary {
          for (pass <- 1 to 3) {
            for (alias <- aliases if alias != author &&
              // XXX
              !author.startsWith("Øistein") &&
              !author.startsWith("Øystein") &&
              !author.endsWith("Hülsbeck") &&
              !Set(
                "Erik 'Carebear' Lydén"
              ).contains(alias) &&
              !Set(
                "Allister Brimble",
                "Anders Hamre",
                "Chris Korte",
                "Jogeir Liljedahl",
                "Peter Salomonsen",
                "Vincent Voois"
              ).contains(author) &&
              !author.startsWith(alias)
            ) {
              val metas = authenticAuthorMetas.getOrElse(Buffer(normalizeAuthor(alias)).sorted.distinct, Set.empty)
              if (metas.exists(m2 => m2.authors.contains(alias) && (m2.hash == updated.hash ||
                  (m2.album.nonEmpty && updated.album.nonEmpty && normalizeAlbum(m2) == normalizeAlbum(updated)) || (pass >= 2 && updated._type.toLowerCase == "compo" && m2._type.toLowerCase == "compo" && updated.year != 0 && m2.year != 0 && m2.year >= updated.year - 1) || (pass >= 3 && updated.publishers.nonEmpty && m2.publishers.nonEmpty && updated.publishers == m2.publishers && m2.year >= updated.year - 1)))) {
                debug(s"Replacing real name authors with non-real names for ${updated} -> ${alias}")
                authors = authors.map(a => if (a == author) alias else a)
                break()
              }
            }
          }
        }
      }
      if (authors != updated.authors) {
        updated = updated.copy(authors = authors.sorted.distinct)
      }
    }
    // XXX
    if (updated.authors.contains("Chris Huelsbeck")) {
      updated = updated.copy(authors = updated.authors.map(_.replace("Chris Huelsbeck", "Chris Hülsbeck")))
    } else if (updated.authors.contains("Jesper Soundwave Kyd")) {
      updated = updated.copy(authors = updated.authors.map(_.replace("Jesper Soundwave Kyd", "Jesper Kyd")))
    }

    updated
  })
  .seq.toBuffer
}
