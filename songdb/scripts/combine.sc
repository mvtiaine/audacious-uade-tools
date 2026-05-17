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
import exodos._
import songlengths._
import tosec._
import whdload._
import wikipedia._


def trace(msg: Unit => String): Unit = {
  //System.err.println(s"TRACE: ${msg(())}")
}

def debug(msg: String): Unit = {
  System.err.println(s"DEBUG: $msg")
}

def warn(msg: String): Unit = {
  System.err.println(s"WARN: $msg")
}

extension (s: String)
  def startsOrEndsWith(s2: String, minSLength: Int = 4, minELength: Int = 7): Boolean =
    s.nonEmpty && s2.nonEmpty && (s == s2 ||
      (s.length >= minSLength && s2.length >= minSLength && (
        s.startsWith(s2) || s2.startsWith(s))) ||
      (s.length >= minELength && s2.length >= minELength && (
        s.endsWith(s2) || s2.endsWith(s))))

def isPreview(lcalbum: String): Boolean =
  !lcalbum.startsWith("game ") && (lcalbum.endsWith(" preview") || lcalbum.endsWith(" prev") || lcalbum.endsWith(" demo") || lcalbum.endsWith(" beta") || lcalbum.endsWith(" (preview)") || lcalbum.endsWith(" (demo)") || lcalbum.endsWith(" (beta)") || lcalbum.endsWith(" version)"))

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

def isRealName(a: String): Boolean = {
  unexotica.composer_handles.contains(a) || amp.composer_handles.contains(a)
}

val authorVariantsCache = new ConcurrentHashMap[String, Buffer[String]]().asScala
def getAuthorVariants(a: String, knownAuthors: Set[String]): Buffer[String] = {
  if (a.isEmpty) return Buffer.empty
  if (authorVariantsCache.contains(a)) return authorVariantsCache(a)
  val normA = normalizeAuthor(a)
  var v = Buffer(a)
  
  val amp_all = amp.all_aliases.getOrElse(normA, Buffer.empty).filter(knownAuthors.contains)
  val demozoo_all = demozoo.all_aliases.getOrElse(normA, Buffer.empty).filter(knownAuthors.contains)
  val unexotica_all = unexotica.all_aliases.getOrElse(normA, Buffer.empty).filter(knownAuthors.contains)
  val heads = (Seq(amp_all.headOption, demozoo_all.headOption, unexotica_all.headOption).flatten).distinct

  v ++= heads
  v ++= amp_all.filterNot(heads.contains)
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
      v1.intersect(v2).nonEmpty
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

def hasRealNames(authors: Buffer[String]): Boolean = {
  authors.exists(a => unexotica.composer_handles.contains(a) || amp.composer_handles.contains(a))
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
      meta.copy(publishers = Buffer.empty, album = "")
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
  val (maxYear, _type, _platform, sources) = md5Constraints.getOrElse(meta.hash, (Int.MaxValue, "", "", Set.empty))
  var filtered = meta
  if (filtered.year > Math.max(maxYear, maxYear + 1)) {
    warn(s"Invalid year ${meta.year} for ${meta}, max year from sources is ${maxYear+1} sources: ${sources.mkString(", ")}")
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

def combineMetadata(
  amp: Buffer[MetaData],
  modland: Buffer[MetaData],
  unexotica: Buffer[MetaData],
  demozoo: Buffer[MetaData],
  oldexotica: Buffer[MetaData],
  wantedteam: Buffer[MetaData],
  modsanthology: Buffer[MetaData],
  fujiology: Buffer[MetaData],
  tosecmusic: Buffer[MetaData], // only supplementary
  leftovers: Buffer[MetaData], // only supplementary
): Buffer[MetaData] = {
  val hashes = (
    amp.par.map(_.hash) ++
    modland.par.map(_.hash) ++
    unexotica.par.map(_.hash) ++
    demozoo.par.map(_.hash) ++
    oldexotica.par.map(_.hash) ++
    wantedteam.par.map(_.hash) ++
    modsanthology.par.map(_.hash) ++
    fujiology.par.map(_.hash) ++
    tosecmusic.par.map(_.hash)
  ).toSet

  val fujiology2 = removeCompilations(fujiology)

  val demozoog = filterByConstraints(demozoo).groupBy(_.hash).par.mapValues(v => v.head.copy(
    album = v.head.album
    .replaceAll(".* - Different Version$", "")
    .replaceAll(".* \\(demo\\)$", "")
    .replaceAll(".* \\(40k\\)$", "")
  )).seq
  val ampg = filterByConstraints(amp).groupBy(_.hash).par.mapValues(_.head).seq
  // canonize Falcon (PL) -> Falcon etc.
  val modlandg = filterByConstraints(modland).groupBy(_.hash).par.mapValues(v => v.head.copy(
    authors = v.head.authors.map(_.replaceAll(" \\(.*\\)$", "")))).seq
  val unexoticag = filterByConstraints(unexotica).groupBy(_.hash).par.mapValues(_.head).seq
  val oldexoticag = filterByConstraints(oldexotica).groupBy(_.hash).par.mapValues(_.head).seq
  val wantedteamg = filterByConstraints(wantedteam).groupBy(_.hash).par.mapValues(_.head).seq
  // canonize XXX of YYY -> XXX
  // XXX.sweden -> XXX etc.
  val modsanthologyg = filterByConstraints(modsanthology).groupBy(_.hash).par.mapValues(v => v.head.copy(
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
  val fujiologyg = filterByConstraints(fujiology2).groupBy(_.hash).par.mapValues(_.head).seq
  //val tosecmusicg = tosecmusic.groupBy(_.hash).par.mapValues(_.head).seq // too unreliable

  // authors: AMP > Demozoo > Modland > UnExotica > OldExotica > WantedTeam > ModsAnthology > Fujiology
  val authorSources = Seq(
    ampg,
    unexoticag,
    demozoog,
    modlandg,
    oldexoticag,
    wantedteamg,
    modsanthologyg,
    fujiologyg,
    // tosecmusicg, // too unreliable
  )

  var extraMetas = Set.empty[MetaData] ++ (
    (amp ++ modland ++ unexotica ++ demozoo ++ oldexotica ++ wantedteam ++ modsanthology ++ fujiology2 ++ leftovers).map(_.copy(hash = ""))
    ++
    tosecMetas ++ whdloadMetas ++ demozooMetas ++ exodosMetas ++ wikipediaMetas)
  val tmpMetas = extraMetas.filter(e => e.publishers.nonEmpty && e.album.nonEmpty).map(e => (normalizeAlbum(e), e.publishers.map(normalizePublisher).distinct, e.year)).toSet
  extraMetas = extraMetas ++
    // XXX unreliable metadata filter out conflicting ones
    tosecmusic.par.map(m => {
      if (m.album.nonEmpty && m.publishers.nonEmpty) {
        val normAlbum = normalizeAlbum(m)
        val normPublishers = m.publishers.map(normalizePublisher)
        var publishers = m.publishers
        var year = m.year
        val metas = tmpMetas.filter(e => e._1 == normAlbum)
        if (year != 0 && metas.exists(e => e._2.exists(p => normPublishers.exists(_.startsOrEndsWith(p))) && e._3 != m.year && e._3 != 0)) {
          trace(_ => s"TOSEC MUSIC: ${m} conflicting year ${m.year} vs other sources, removing year")
          year = 0
        }
        if (metas.exists(e => normPublishers.exists(p => !e._2.exists(_.startsOrEndsWith(p))))) {
          trace(_ => s"TOSEC MUSIC: ${m} conflicting publishers ${m.publishers.mkString(", ")} vs other sources, removing publishers")
          publishers = Buffer.empty
        }
        m.copy(hash = "", publishers = publishers, year = year)
      } else m.copy(hash = "")
    })
    .seq
  extraMetas = extraMetas
    .filterNot(m => (m._type == "Game" && m._platform == "PC" && (m.year > 0 && m.year <= 1991)))
    .filterNot(m => (m._platform == "PC" && (m.year > 0 && m.year < 1990)))
    .filterNot(m => (m._platform == "Atari" && (m.year > 0 && m.year < 1989)))
    // XXX
    .filterNot(m => (m._type == "Game" && m._platform == "PC" && m.album == "The Thing" && m.year == 2002))
    .filterNot(m => m.authors.isEmpty && m.album.isEmpty && m.publishers.isEmpty) // only year
    .filterNot(m => m.authors.isEmpty && m.album.isEmpty && m.year == 0) // only publishers
    .filterNot(m => m.authors.isEmpty && m.publishers.isEmpty && m.year == 0) // only album
    //.filterNot(m => m.album.isEmpty && m.publishers.isEmpty && m.year == 0) // only authors

  val authenticAuthorMetas = (authorSources
    .flatMap(_.values.filter(m => m.authors.nonEmpty).map(m => (m.authors.map(normalizeAuthor).sorted.distinct, m)))
    ++
    extraMetas.filter(m => m.authors.nonEmpty).map(m => (m.authors.map(normalizeAuthor).sorted.distinct, m)))
    .groupBy(_._1)
    .map { case (authors, pairs) => authors -> pairs.map(_._2).toSet }

  val knownAuthors = extraMetas.flatMap(_.authors).toSet
  extraMetas = extraMetas
    .par
    .flatMap(expandArticleVariants)
    .flatMap(m => expandAuthorVariants(m, knownAuthors))
    .seq
  
  val sceneGroups = extraMetas.filter(m => m._type.nonEmpty && m._type.toLowerCase != "game" && m.album.nonEmpty && m.publishers.nonEmpty).flatMap(_.publishers).toSet
    .filterNot(Set("Binary Emotions", "Edge", "Frontier Software", "Imageworks", "Kalisto", "Psygnosis", "Rainbow Arts", "Starbyte", "Thalion").contains)

  var metasByHash = new ConcurrentHashMap[String, MetaData]().asScala

  // album/publishers/year source priority
  val allMetaSources = Seq(
    unexoticag.filter(_._2._type.toLowerCase == "game"),
    oldexoticag.filter(_._2._type.toLowerCase == "game"),
    wantedteamg.filter(_._2._type.toLowerCase == "game"),
    ampg.filter(_._2._type.toLowerCase == "game"),
    fujiologyg.filter(e => e._2._type.toLowerCase == "game" && e._2._platform.toLowerCase != "atari"),
    demozoog.filterNot(e => e._2._type.toLowerCase == "musicdisk" && e._2._platform.toLowerCase == "atari"),
    oldexoticag.filter(_._2._type.toLowerCase != "game"),
    unexoticag.filter(_._2._type.toLowerCase != "game"),
    wantedteamg.filter(_._2._type.toLowerCase != "game"),
    ampg.filter(_._2._type.toLowerCase != "game"),
    modlandg,
    modsanthologyg,
    demozoog.filter(e => e._2._type.toLowerCase == "musicdisk" && e._2._platform.toLowerCase == "atari"),
    fujiologyg.filterNot(e => e._2._type.toLowerCase == "game" && e._2._platform.toLowerCase != "atari"),
  )

  for (pass <- 1 to allMetaSources.size) {

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

    val metaSources = allMetaSources.take(pass)
    val excludedHashes = allMetaSources.drop(pass).flatMap(_.keys).toSet -- metaSources.flatMap(_.keys)
    hashes.par.foreach { hash =>
      val existing = metasByHash.get(hash)
 
      def pickAuthor[T](sources: Seq[Map[String, MetaData]]) =
        if (existing.isDefined && existing.get.authors.nonEmpty) {
          Some(existing.get.authors)
        } else {
          sources.toStream.map(_.get(hash)).find(m =>
            m.isDefined && m.get.authors.nonEmpty).map(_.get.authors)
        }
    
      def pick[T](sources: Seq[Map[String, MetaData]], f: MetaData => T): Option[T] = {
        val picked = sources.toStream.map(_.get(hash)).find(m =>
          // pick only if has some non-author metadata
          m.isDefined && (m.get.publishers.nonEmpty || m.get.album.nonEmpty || m.get.year != 0))
        .map(_.get)
      
        val pScore = if (picked.isDefined) picked.map(e => e.publishers.size + (if (e.album.nonEmpty) 1 else 0) + (if (e.year > 0) 99 else 0)).getOrElse(0) else 0
        val eScore = if (existing.isDefined) existing.map(e => e.publishers.size + (if (e.album.nonEmpty) 1 else 0) + (if (e.year > 0) 99 else 0)).getOrElse(0) else 0

        if (existing.isDefined && (existing.get.publishers.nonEmpty || existing.get.album.nonEmpty || existing.get.year != 0) && (!picked.isDefined || pScore < eScore || (pScore == eScore && (picked.get.year == 0 || picked.get.year >= existing.get.year))))
          existing.map(f)
        else
          picked.map(f)
      }

      val authors = pickAuthor(authorSources).getOrElse(Buffer.empty)
      var album = pick(metaSources, f = _.album).getOrElse("")
      var publishers = pick(metaSources, f = _.publishers).getOrElse(Buffer.empty)
      var year = pick(metaSources, f = _.year).getOrElse(0)
      var _type = pick(metaSources, f = _. _type).getOrElse("")
      var _platform = pick(metaSources, f = _. _platform).getOrElse("")

      var updated = MetaData(hash, authors, publishers, album, year, _type, _platform)

      if (existing.isDefined && existing.get != updated) {
        val typeChange = (existing.get._type.toLowerCase == "game" && updated._type.toLowerCase != "game") || (existing.get._type.toLowerCase != "game" && updated._type.toLowerCase == "game")
        if (typeChange) {
          var isGame = updated._type.toLowerCase == "game"
          var newAuthors = authorSources.toStream.map(_.get(hash)).find(m =>
            m.isDefined && (if (isGame) m.get._type.toLowerCase == "game" else m.get._type.toLowerCase != "game") && m.get.authors.nonEmpty).map(_.get.authors).getOrElse(Buffer.empty)
          if (newAuthors.isEmpty) {
            val metaSource = metaSources.last.get(hash)
            newAuthors = if (metaSource.isDefined && metaSource.get.authors.nonEmpty) metaSource.get.authors else existing.get.authors
          }
          updated = updated.copy(authors = newAuthors)
        }
      }

      trace(_ => s"initial pick for $hash -> ${updated}")
      metasByHash(hash) = updated
    }
  
    val allmetas = metasByHash.values ++ extraMetas

    val authorMetas = allmetas
      .filterNot(_.authors.isEmpty)
      .filterNot(m => m.year == 0 && m.publishers.isEmpty && m.album.isEmpty && m._type.isEmpty && m._platform.isEmpty)
      .par.flatMap(m => {
        m.authors.map(normalizeAuthor).flatMap { author =>
          Set(
            (author, m)
          )
        }
      })
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)

    val yearAlbumPublishers = allmetas
      .filterNot(_.album.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.year, normalizeAlbum(m), m.publishers.map(normalizePublisher).sorted.distinct))
  
    val yearAlbum = allmetas
      .filterNot(_.album.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.year, normalizeAlbum(m)))

    val albumPublishers = allmetas
      .filterNot(_.album.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .map(m => (normalizeAlbum(m), m.publishers.map(normalizePublisher).sorted.distinct))

    val authorsAlbumPublishers = allmetas
      .filterNot(_.authors.isEmpty)
      .filterNot(_.album.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .map(m => (m.authors.map(normalizeAuthor).sorted.distinct, normalizeAlbum(m), m.publishers.map(normalizePublisher).sorted.distinct))
  
    val authorsAlbumYear = allmetas
      .filterNot(_.authors.isEmpty)
      .filterNot(_.album.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.authors.map(normalizeAuthor).sorted.distinct, normalizeAlbum(m), m.year))

    val authorsYearPublishers = allmetas
      .filterNot(_.authors.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.authors.map(normalizeAuthor).sorted.distinct, m.year, m.publishers.map(normalizePublisher).sorted.distinct))

    val authorsYearNoAlbum = allmetas
      .filter(_.album.isEmpty)
      .filterNot(_.authors.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.authors.map(normalizeAuthor).sorted.distinct, m.year))

    val authorsPublishersNoAlbum = allmetas
      .filter(_.album.isEmpty)
      .filterNot(_.authors.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .map(m => (m.authors.map(normalizeAuthor).sorted.distinct, m.publishers.map(normalizePublisher).sorted.distinct))

    metasByHash.values.par.foreach { meta =>
      val hash = meta.hash
      val authors = meta.authors
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
              e._3.exists(p => normPublishers.exists(_.startsOrEndsWith(p)))) &&
              !publishers.head.endsWith(" Party") // XXX
          ) {
            trace(_ => s"pickYearAlbumWithPublishers: $hash -> ${m.get.year} + ${m.get.album}")
            year = m.get.year
            album = m.get.album
          }
        }
      }

      def pickYearAlbumWithoutPublishers(m: Option[MetaData]) = {
        if (year == 0 && album.isEmpty && m.isDefined && m.get.year != 0 && m.get.album.nonEmpty) {
          if (publishers.isEmpty && yearAlbum.exists(e => e._1 == m.get.year &&
              e._2 == normalizeAlbum(m.get))
           ) {
            trace(_ => s"pickYearAlbumWithoutPublishers: $hash -> ${m.get.year} + ${m.get.album}")
            year = m.get.year
            album = m.get.album
          }
        }
      }

      def pickAlbumPublishersWithYear(m: Option[MetaData]) = {
        if (album.isEmpty && publishers.isEmpty && m.isDefined && m.get.album.nonEmpty && m.get.publishers.nonEmpty) {
          val mNormPubs = m.get.publishers.map(normalizePublisher)
          val mNormAlb = normalizeAlbum(m.get)
          if (year != 0 && yearAlbumPublishers.exists(e => e._1 == year &&
              e._2 == mNormAlb &&
              e._3.exists(p => mNormPubs.exists(_.startsOrEndsWith(p))))
          ) {
            trace(_ => s"pickAlbumPublishersWithYear: $hash -> ${m.get.album} + ${m.get.publishers}")
            album = m.get.album
            publishers = m.get.publishers
          }
        }
      }

      def pickAlbumPublishersWithoutYear(m: Option[MetaData]) = {
        if (album.isEmpty && publishers.isEmpty && m.isDefined && m.get.album.nonEmpty && m.get.publishers.nonEmpty) {
          val mNormPubs = m.get.publishers.map(normalizePublisher)
          val mNormAlb = normalizeAlbum(m.get)
          if (year == 0 && albumPublishers.exists(e => e._1 == mNormAlb &&
              e._2.exists(p => mNormPubs.exists(_.startsOrEndsWith(p))))
           ) {
            trace(_ => s"pickAlbumPublishersWithoutYear: $hash -> ${m.get.album} + ${m.get.publishers}")
            album = m.get.album
            publishers = m.get.publishers
          }
        }
      }

      def pickAlbumWithPublishersAndYear(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty) {
          if (year != 0 && publishers.nonEmpty && yearAlbumPublishers.exists(e => e._1 == year &&
              e._2 == normalizeAlbum(m.get) &&
              e._3.exists(p => normPublishers.exists(_.startsOrEndsWith(p)))) &&
              !publishers.head.endsWith(" Party") // XXX
          ) {
            trace(_ => s"pickAlbumWithPublishersAndYear: $hash -> ${m.get.album}")
            album = m.get.album
          }
        }
      }

      def pickAlbumWithPublishers(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty) {
          if (year == 0 && publishers.nonEmpty && albumPublishers.exists(e =>
              e._1 == normalizeAlbum(m.get) &&
              e._2.exists(p => normPublishers.exists(_.startsOrEndsWith(p)))) &&
              !publishers.head.endsWith(" Party") // XXX
          ) {
            trace(_ => s"pickAlbumWithPublishers: $hash -> ${m.get.album}")
            album = m.get.album
          }
        }
      }

      def pickAlbumWithYear(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty) {
          if (publishers.isEmpty && year != 0 && yearAlbum.exists(e => e._1 == year &&
              e._2 == normalizeAlbum(m.get))
           ) {
            trace(_ => s"pickAlbumWithYear: $hash -> ${m.get.album}")
            album = m.get.album
          }
        }
      }

      def pickPublishersWithAlbumAndYear(m: Option[MetaData]): Unit = {
        if (publishers.isEmpty && m.isDefined && m.get.publishers.nonEmpty) {
          val mNormPubs = m.get.publishers.map(normalizePublisher)
          val mNormAlbPub = normalizeAlbum(_type, album, m.get.publishers)
          if (year != 0 && album.nonEmpty && yearAlbumPublishers.exists(e => e._1 == year &&
              e._2 == mNormAlbPub &&
              e._3.exists(p => mNormPubs.exists(_.startsOrEndsWith(p))))
          ) {
            trace(_ => s"pickPublishersWithAlbumAndYear: $hash -> ${m.get.publishers}")
            publishers = m.get.publishers
          }
        }
      }

      def pickPublishersWithAlbum(m: Option[MetaData]): Unit = {
        if (publishers.isEmpty && m.isDefined && m.get.publishers.nonEmpty) {
          val mNormPubs = m.get.publishers.map(normalizePublisher)
          val mNormAlbPub = normalizeAlbum(_type, album, m.get.publishers)
          if (year == 0 && album.nonEmpty && albumPublishers.exists(e =>
              e._1 == mNormAlbPub &&
              e._2.exists(p => mNormPubs.exists(_.startsOrEndsWith(p))))
          ) {
            trace(_ => s"pickPublishersWithAlbum: $hash -> ${m.get.publishers}")
            publishers = m.get.publishers
          }
        }
      }

      def pickYearWithAlbumAndPublishers(m: Option[MetaData]) = {
        if (year == 0 && m.isDefined && m.get.year != 0) {
          if (album.nonEmpty && publishers.nonEmpty && yearAlbumPublishers.exists(e => e._1 == m.get.year &&
              e._2 == normalizeAlbum(_type, album, publishers) &&
              e._3.exists(p => normPublishers.exists(_.startsOrEndsWith(p)))) &&
              !publishers.head.endsWith(" Party") // XXX
          ) {
            trace(_ => s"pickYearWithAlbumAndPublishers: $hash -> ${m.get.year}")
            year = m.get.year
          }
        }
      }

      def pickYearWithAlbum(m: Option[MetaData]) = {
        if (year == 0 && m.isDefined && m.get.year != 0) {
          if (publishers.isEmpty && album.nonEmpty && yearAlbum.exists(e => e._1 == m.get.year &&
              e._2 == (normalizeAlbum(_type, album, Buffer.empty)))
           ) {
            trace(_ => s"pickYearWithAlbum: $hash -> ${m.get.year}")
            year = m.get.year
          }
        }
      }

      def pickAlbumWithAuthorsYearPublishers(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty) {
          if (authors.nonEmpty && year != 0 && publishers.nonEmpty &&
              authorsYearPublishers.exists(e =>
                e._1.exists(a => normAuthors.exists(_ == a)) &&
                e._2 == year &&
                e._3.exists(p => normPublishers.exists(_.startsOrEndsWith(p)))
              ) &&
              authorsAlbumPublishers.exists(e =>
                e._1.exists(a => normAuthors.exists(_ == a)) &&
                e._2 == normalizeAlbum(m.get) &&
                e._3.exists(p => publishers.map(normalizePublisher).exists(_.startsOrEndsWith(p)))
              ) &&
              authorsAlbumYear.exists(e =>
                e._1.exists(a => normAuthors.exists(_ == a)) &&
                e._2 == normalizeAlbum(m.get) &&
                e._3 == year) &&
              !publishers.head.endsWith(" Party") // XXX
          ) {
            trace(_ => s"pickAlbumWithAuthorsYearPublishers: $hash -> ${m.get.album}")
            album = m.get.album
          }
        }
      }

      def pickAlbumWithAuthorsYear(m: Option[MetaData]) = {
        if (album.isEmpty && publishers.isEmpty && m.isDefined && m.get.album.nonEmpty && m.get.  publishers.isEmpty) {
          if (authors.nonEmpty && year == m.get.year && authorsAlbumYear.exists(e =>
              e._1.exists(a => normAuthors.exists(_ == a)) &&
              e._2 == normalizeAlbum(m.get) &&
              e._3 == year)
          ) {
            trace(_ => s"pickAlbumWithAuthorsYear: $hash -> ${m.get.album}")
            album = m.get.album
          }
        }
      }

      def pickAlbumWithAuthorsPublishers(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty && m.get.year == year) {
          if (authors.nonEmpty && publishers.nonEmpty && authorsAlbumPublishers.exists(e =>
              e._1.exists(a => normAuthors.exists(_ == a)) &&
              e._2 == normalizeAlbum(m.get) &&
              e._3.exists(p => publishers.map(normalizePublisher).exists(_.startsOrEndsWith(p)))) &&
              !publishers.head.endsWith(" Party") // XXX
          ) {
            trace(_ => s"pickAlbumWithAuthorsPublishers: $hash -> ${m.get.album}")
            album = m.get.album
          }
        }
      }

      def pickYearPublishersWithoutAlbum(m: Option[MetaData]) = {
        if (year == 0 && publishers.isEmpty && album.isEmpty && m.isDefined && m.get.year != 0 && m.get.publishers.nonEmpty) {
          val mNormPubs = m.get.publishers.map(normalizePublisher)
          if (authors.nonEmpty && m.get.album.isEmpty &&
              authorsYearPublishers.exists(e =>
                e._1.exists(a => normAuthors.exists(_ == a)) &&
                e._2 == m.get.year &&
                e._3.exists(p => mNormPubs.exists(_.startsOrEndsWith(p)))
              )
          ) {
            trace(_ => s"pickYearPublishersWithoutAlbum: $hash -> ${m.get.year} + ${m.get.publishers}")
            year = m.get.year
            publishers = m.get.publishers
          }
        }
      }

      def pickYearWithoutAlbum(m: Option[MetaData]) = {
        if (year == 0 && album.isEmpty && m.isDefined && m.get.year != 0) {
          if (authors.nonEmpty && m.get.album.isEmpty &&
              authorsYearNoAlbum.exists(e =>
                e._1.exists(a => normAuthors.exists(_ == a)) &&
                e._2 == m.get.year)
          ) {
            trace(_ => s"pickYearWithoutAlbum: $hash -> ${m.get.year}")
            year = m.get.year
          }
        }
      }

      def pickPublishersWithoutAlbum(m: Option[MetaData]) = {
        if (publishers.isEmpty && album.isEmpty && m.isDefined && m.get.publishers.nonEmpty && m.get.year == year) {
          val mNormPubs = m.get.publishers.map(normalizePublisher)
          if (authors.nonEmpty && m.get.album.isEmpty &&
              authorsPublishersNoAlbum.exists(e =>
                e._1.exists(a => normAuthors.exists(_ == a)) &&
                e._2.exists(p => mNormPubs.exists(_.startsOrEndsWith(p)))
              )
          ) {
            trace(_ => s"pickPublishersWithoutAlbum: $hash -> ${m.get.publishers}")
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
      sources.foreach(pickAlbumWithYear)

      sources.foreach(pickPublishersWithAlbumAndYear)
      sources.foreach(pickPublishersWithAlbum)

      sources.foreach(pickYearWithAlbumAndPublishers)
      sources.foreach(pickYearWithAlbum)

      sources.foreach(pickAlbumWithAuthorsYearPublishers)
      sources.foreach(pickAlbumWithAuthorsYear)
      sources.foreach(pickAlbumWithAuthorsPublishers)

      sources.foreach(pickYearPublishersWithoutAlbum)
      sources.foreach(pickYearWithoutAlbum)
      sources.foreach(pickPublishersWithoutAlbum)

      val updated = MetaData(hash, authors, publishers, album, year, _type, _platform)
      trace(_ =>s"after pick for $hash -> ${updated}")
      metasByHash(hash) = updated
    }
  
    val processAgain = pass == allMetaSources.size

    // find metas which have common author(s) + album, add publishers and year if missing
    val metasByAuthorAlbumWithPublisherOrYear = (metasByHash.values ++ extraMetas)
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
      .mapValues(_.flatMap(_._2).toSet)

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
            val filtered = metas.filter(m2 => m2._type.isEmpty || (m._type.toLowerCase.startsWith("game") && m2._type.toLowerCase.startsWith("game")) || (!m._type.toLowerCase.startsWith("game") && !m2._type.toLowerCase.startsWith("game")))
            metas = filtered
          }
          if (m._platform.nonEmpty) {
            val filtered = metas.filter(m2 => m2._platform.isEmpty || m2._platform.toLowerCase == m._platform.toLowerCase)
            metas = filtered
          }
          var publishers = if (m.publishers.isEmpty) pickMostCommonPublishers(metas) else m.publishers
          if (!metas.forall(m => m.publishers.isEmpty
            || m.publishers.map(normalizePublisher).exists(p => publishers.map(normalizePublisher).exists(_.startsOrEndsWith(p))))
          ) {
            warn(s"(1) publishers differ for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.publishers.mkString(",")} != ${metas.flatMap(_.publishers).mkString(",")}")
          }
          // TODO tag source + exclude/override modsanthology year
          var year = if (m.year == 0) pickMostCommonYear(metas) else m.year
          if (!metas.forall(m => m.year == 0 || m.year == year)) {
            warn(s"(1) year differs for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.year} != ${metas.map(_.year).mkString(",")}")
            year = m.year
          }
          if (metas.nonEmpty && (publishers != m.publishers || year != m.year)) {
            debug(s"(1) overriding metadata for ${m} -  publishers ${m.publishers.mkString(",")} -> ${publishers.mkString(",")}, year ${m.year} -> ${year}")
            trace(_ => s"(1) ${m.hash} metas: ${metas.seq} key: ${key}")
            metasByHash(m.hash) = m.copy(publishers = publishers, year = year)
          }
        }
      }
    )
  
    // find metas which have common publisher(s) + album, add year if missing
    val metasByPublisherAlbumWithYear = (metasByHash.values ++ extraMetas)
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
      .mapValues(_.flatMap(_._2).toSet)

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
            val filtered = metas.filter(m2 => m2._type.isEmpty || (m._type.toLowerCase.startsWith("game") && m2._type.toLowerCase.startsWith("game")) || (!m._type.toLowerCase.startsWith("game") && !m2._type.toLowerCase.startsWith("game")))
            metas = filtered
          }
          if (m._platform.nonEmpty) {
            val filtered = metas.filter(m2 => m2._platform.isEmpty || m2._platform.toLowerCase == m._platform.toLowerCase)
            metas = filtered
          }
          // TODO tag source + exclude/override modsanthology year
          var year = if (m.year == 0) pickMostCommonYear(metas) else m.year
          if (!metas.forall(m => m.year == 0 || m.year == year)) {
            warn(s"(2) year differs for ${m.hash} - ${m.album} - ${m.publishers.mkString(",")} - ${m.year} != ${metas.map(_.year).mkString(",")}")
          } else if (metas.nonEmpty && year != m.year) {
            debug(s"(2) overriding year for ${m} - year ${m.year} -> ${year}")
            trace(_ => s"(2) ${m.hash} metas: ${metas.seq} key: ${key}")
            metasByHash(m.hash) = m.copy(year = year)
          }
        }
      }
    )

    // if meta author is missing, compare to other metas
    // and when there is only 1 album with same non-empty name and only 1 distinct author(s) for that album and publisher matches (or is missing in the original meta)
    // -> add author, publisher and year
    val metasByAlbumWithAuthorPublisherOrYear = (metasByHash.values ++ extraMetas)
      .filterNot(_.album.isEmpty)
      .filterNot(m => m.publishers.isEmpty && m.year == 0)
      .groupBy(m => normalizeAlbum(m))

    metasByHash.values.par.foreach(m => {
      if (!(m.authors.nonEmpty || m.album.isEmpty || (m.publishers.nonEmpty && m.year != 0) ||
           (m.album.nonEmpty && m.authors.isEmpty && m.publishers.isEmpty && m.year == 0))) {
        val key = normalizeAlbum(m)
        trace(_ => s"(3) ${m.hash} key: ${key}")
        var metas = metasByAlbumWithAuthorPublisherOrYear.get(key)
        if (m._type.nonEmpty && metas.isDefined) {
          val filtered = metas.get.filter(m2 => m2._type.isEmpty || (m._type.toLowerCase.startsWith("game") && m2._type.toLowerCase.startsWith("game")) || (!m._type.toLowerCase.startsWith("game") && !m2._type.toLowerCase.startsWith("game")))
          metas = if (filtered.nonEmpty) Some(filtered) else None
        }
        if (m._platform.nonEmpty && metas.isDefined) {
          val filtered = metas.get.filter(m2 => m2._platform.isEmpty || m2._platform.toLowerCase == m._platform.toLowerCase)
          metas = if (filtered.nonEmpty) Some(filtered) else None
        }
        if (metas.isDefined && metas.get.size >= 1) {
          val authors = {
            val grouped = metas.get.groupBy(_.authors.sorted)
            grouped.maxBy(_._2.size)._1
          }
          if (metas.get.forall(_.authors.map(normalizeAuthor).exists(a => authors.map(normalizeAuthor).exists(_ == a)))) {
            var publishers = if (m.publishers.isEmpty) pickMostCommonPublishers(metas.get) else m.publishers
            if (!metas.get.forall(m => m.publishers.isEmpty
                || m.publishers.map(normalizePublisher).exists(p => publishers.map(normalizePublisher).exists(_.startsOrEndsWith(p))))
            ) {
              warn(s"(3) publishers differ for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.publishers.mkString(",")} != ${metas.get.flatMap(_.publishers).mkString(",")}")
            }
            // TODO tag source + exclude/override modsanthology year
            var year = if (m.year == 0) pickMostCommonYear(metas.get) else m.year
            if (!metas.get.forall(m => m.year == 0 || m.year == year)) {
              warn(s"(3) year differs for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.year} != ${metas.get.map(_.year).mkString(",")}")
            }
            if (authors != m.authors || publishers != m.publishers || year != m.year) {
              debug(s"(3) overriding metadata for ${m} - publishers ${m.publishers.mkString(",")} -> ${publishers.mkString(",")}, year ${m.year} -> ${year}")
              trace(_ => s"(3) ${m.hash} metas: ${metas.get.seq} key: ${key}")
              metasByHash(m.hash) = m.copy(authors = authors, publishers = publishers, year = year)
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
              lazy val compatibleMetas = metas.forall(m2 => (m2.publishers.isEmpty || m.publishers.isEmpty || m.publishers.map(normalizePublisher).exists(p => m2.publishers.map(normalizePublisher).exists(_.startsOrEndsWith(p)))) && (m.album.isEmpty || m2.album.isEmpty || normalizeAlbum(m) == normalizeAlbum(m2)) && (m.year == 0 || m2.year == 0 || m.year == m2.year))
              lazy val earlierYear = metas.exists(m2 => m.year > 0 && m2.year > 0 && m2.year < m.year)
              lazy val normAuthors = m.authors.map(normalizeAuthor).sorted.distinct
              lazy val authorMetas = authenticAuthorMetas.getOrElse(normAuthors, Set.empty).filter(m2 => m2.hash.nonEmpty && m2.authors.map(normalizeAuthor).sorted.distinct == normAuthors && m2.year == m.year)
              lazy val lonelyMeta = authorMetas.isEmpty || authorMetas.forall(m2 => m2.publishers == m.publishers && m2.album == m.album)
              val meta = keepAuthor(m)
              if (meta.isDefined && compatibleAuthors && ((compatibleMetas && lonelyMeta) || earlierYear)) {
                debug(s"Overriding meta data entry ${m} with ${meta.get} because does not pass constraint filter - metas: ${metas}, authorMetas: ${authorMetas} compatibleAuthors: ${compatibleAuthors}, compatibleMetas: ${compatibleMetas}, lonelyMeta: ${lonelyMeta}, earlierYear: ${earlierYear}")
                metasByHash(m.hash) = meta.get
              } else if (!meta.isDefined && compatibleMetas) {
                debug(s"Removing meta data entry ${m} because does not pass constraint filter and has no authors - metas: ${metas}")
                metasByHash.remove(m.hash)
              } else {
                debug(s"Keeping meta data entry ${m} - metas: ${metas}, compatibleAuthors: ${compatibleAuthors}, compatibleMetas: ${compatibleMetas}, authorMetas: ${authorMetas}, lonelyMeta: ${lonelyMeta}, earlierYear: ${earlierYear}")
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
            val validSourceMetas = if (rawMinyear < 9999) sourceMetas.filter(e => e.year > 0 && e.year <= rawMinyear + 1) else sourceMetas
            val scores = duplicateMetas.map(e => (e.hash, (if (e.authors.nonEmpty) 100 else 0) + (if (e.publishers.nonEmpty) 10 else 0) + (if (e.album.nonEmpty) 1000 else 0) + (if (e.year > 0) 10000 else 0))).toMap
            val sourceScores = validSourceMetas.map(e => (e.hash, scores.getOrElse(e.hash, 0))).toMap
            val bestscore = if (sourceScores.isEmpty) 0 else sourceScores.values.max
            val bestmetas = validSourceMetas.filter(e => sourceScores.getOrElse(e.hash, 0) == bestscore)
            val minyear =
              if (bestmetas.isEmpty) 9999
              else bestmetas.map(e => if (e.year > 0) e.year else 9999).min
            val maxauthors = if (bestmetas.isEmpty) 0 else bestmetas.filter(m => (m.year == 0 && minyear == 9999) || m.year <= minyear).map(_.authors.size).max
            val maxpublishers = if (bestmetas.isEmpty) 0 else bestmetas.filter(m => (m.year == 0 && minyear == 9999) || m.year <= minyear).map(_.publishers.size).max
            val byyear = bestmetas.filter(m => (m.year == 0 && minyear == 9999) || m.year <= minyear)
            val byauthor = byyear.filter(_.authors.size == maxauthors)
            val bypublishers = if (byauthor.isEmpty) byyear.filter(_.publishers.size == maxpublishers || maxpublishers == 0) else byauthor.filter(_.publishers.size == maxpublishers || maxpublishers == 0)
            var bests = if (bypublishers.nonEmpty) bypublishers else if (byauthor.nonEmpty) byauthor else byyear
            bests = bests.sortBy(b => if (b.album.isEmpty) "ZZZZZ" else normalizeAlbum(b))
            // pick majority publisher/album combination matching minyear
            def pickBySourcePriority(
              candidates: Iterable[MetaData]
            ): Option[MetaData] = {
              authorSources.iterator.map { source =>
                val matches = candidates.filter(c => source.get(c.hash).exists(src =>
                  (src.authors.map(normalizeAuthor).exists(a => c.authors.map(normalizeAuthor).exists(_ == a))) &&
                  (src.publishers == c.publishers || src.publishers.map(normalizePublisher).exists(p => c.publishers.map(normalizePublisher).exists(_.startsOrEndsWith(p)))) &&
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
            debug(s"Combining ${duplicateHashes.mkString(", ")} to ${best.hash} with score ${bestscore} (${scores.map(e => s"${e._1}:${e._2}").mkString(", ")}) duplicate metas: ${duplicateMetas.mkString(" | ")} best: ${best} bestscore: ${bestscore} bests: ${bests.mkString(" | ")} bestmetas: ${bestmetas.mkString(" | ")}")

            // build a subsong count map for all candidates using songlengths
            val subsongCountMap: Map[String, Int] = cachedDups.map { h =>
              val entries = songlengths.songlengthsByMd5(h)
              h -> entries.headOption.map(_.subsongs.size).getOrElse(-1)
            }.toMap
            // build an ordered candidate list: try the selected `best` first, then other candidates by score
            val candidatesGrouped = duplicateMetas.distinct.sortBy(m => (-scores.getOrElse(m.hash, 0), m.hash))
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
              .sortBy(m => (-scores.getOrElse(m.hash, 0), if (m.album.isEmpty) "ZZZZZ" else normalizeAlbum(m), m.hash))
            // compute per-candidate fallback authors once (avoid recomputing per-hash)
            lazy val candAuthorsMap: Map[String, Buffer[String]] = (Seq(best) ++ candidatesSorted).map { cand =>
              val authors = if (cand.authors.nonEmpty) cand.authors else {
                val authorGroups = duplicateMetas.groupBy(_.authors.sorted).filter(_._1.nonEmpty)
                if (authorGroups.nonEmpty) {
                  val maxCount = authorGroups.values.map(_.size).max
                  val tiedAuthors = authorGroups.filter(_._2.size == maxCount).keys.toSeq.sortBy(_.mkString(","))
                  authorSources.iterator.map { source =>
                    tiedAuthors.find { authors =>
                      duplicateHashes.exists(hash => source.get(hash).exists(_.authors.map(normalizeAuthor).exists(a => authors.map(normalizeAuthor).exists(_ == a))))
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
              val hashScore = scores.getOrElse(hash, 0)
              val hashSubsongCount = subsongCountMap.getOrElse(hash, -1)
              val hashPlatform = if (meta.isDefined) meta.get._platform else ""
              val hashType = if (meta.isDefined) meta.get._type else ""
              val allowOverride = !meta.isDefined || (cachedDups.exists(h => !_filter(meta.get.copy(hash = h)) && subsongCountMap.getOrElse(h, -1) == hashSubsongCount) && candidatesSorted.forall(c => areAuthorsCompatible(meta.get.authors, c.authors, knownAuthors)))
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
                .filter(c => allowOverride || scores.getOrElse(c.hash, 0) >= hashScore)
                .filter(c => _filter(c.copy(hash = hash)))
                .distinctBy(c => (subsongCountMap.getOrElse(c.hash, -1), c.authors, c.publishers, c.album, c.year, c._type, c._platform))
                .sortBy { cand =>
                  val candSubsongCount = subsongCountMap.getOrElse(cand.hash, -1)
                  val subsongMatch = if (hashSubsongCount > 0 && candSubsongCount > 0) if (hashSubsongCount == candSubsongCount) -1 else 0 else 0
                  val platformMatch = if (hashPlatform.nonEmpty && cand._platform.nonEmpty) if (hashPlatform.toLowerCase == cand._platform.toLowerCase) -1 else 0 else 0
                  val typeMatch = if (hashType.nonEmpty && cand._type.nonEmpty) (if (hashType.toLowerCase == cand._type.toLowerCase) -1 else 0) else 0
                  val sourcePrio = authorSources.indexWhere(_.contains(cand.hash))
                  val sourceCount = sourceCounts.getOrElse(cand.hash, 0)
                  val year = if (yearMissing) constraintYear(cand) else if (cand.year > 0) cand.year else 9999
                  ((subsongMatch, platformMatch, typeMatch), if (cand.copy(hash = "") == best.copy(hash = "")) -1 else 0, (-scores.getOrElse(cand.hash, 0), -formatScore(cand)), (sourcePrio, -sourceCount), (year, if (cand.album.isEmpty) "ZZZZZ" else normalizeAlbum(cand), cand.hash))
                }
                .distinct
              trace(_ => s"Candidates for hash ${hash} meta: ${meta} by priority: ${candsByPriority} candsSorted: ${candidatesSorted} allowOverride: ${allowOverride}")
              candsByPriority.foreach { cand =>
                val different = meta.isEmpty || meta.get.copy(hash = "") != cand.copy(hash = "")
                if (!applied && different) {
                  lazy val candAuthors = candAuthorsMap.getOrElse(cand.hash, Buffer.empty)
                  lazy val candNormAuthors = candAuthors.flatMap(a => getAuthorVariants(a, knownAuthors)).map(normalizeAuthor)
                  lazy val candNormPublishers = cand.publishers.map(normalizePublisher)
                  lazy val candNormAlbum = normalizeAlbum(cand)
                  lazy val candOk = (allowOverride || meta.get.authors.isEmpty ||
                    normAuthors.intersect(candNormAuthors).nonEmpty ||
                    {
                      val metas = meta.get.authors.map(normalizeAuthor).flatMap(a => authorMetas.get(a)).flatten
                      if (metas.nonEmpty) {
                        val knownYears = metas.map(_.year).filter(_ > 0)
                        (cand.year == 0 || (knownYears.nonEmpty && cand.year >= knownYears.min - 1 && cand.year <= knownYears.max + 1)) &&
                        (candNormAlbum.isEmpty || metas.exists(m => normalizeAlbum(m) == candNormAlbum)) &&
                        (candNormPublishers.isEmpty || metas.exists(m => m.publishers.map(normalizePublisher).exists(p => candNormPublishers.exists(_.startsOrEndsWith(p)))))
                      } else false
                    }) && (allowOverride || meta.get._type.isEmpty || cand._type.nonEmpty)
                       && (allowOverride || meta.get.year == 0 || cand.year <= meta.get.year + 1)

                  lazy val candScore = scores.getOrElse(cand.hash, 0)
                  lazy val isCandPreview = isPreview(cand.album)

                  if ((allowOverride || hashScore < candScore ||
                    (hashScore == candScore && cand.year > 0 && cand.year < meta.get.year) ||
                    (hashScore == candScore && !isCandPreview && isPreview(meta.get.album) && candNormAlbum == normalizeAlbum(meta.get)))
                    && (candOk || allowOverride)) {
                    val old = meta.getOrElse(cand)
                    lazy val typeChange = (old._type.toLowerCase == "game" && cand._type.toLowerCase != "game") || (old._type.toLowerCase != "game" && cand._type.toLowerCase == "game")
                    lazy val differentNames = old.authors.map(normalizeAuthor).intersect(candAuthors.map(normalizeAuthor)).isEmpty
                    lazy val compatibleAuthors = areAuthorsCompatible(old.authors, candAuthors, knownAuthors)
                    lazy val authors =
                      if (old.authors.isEmpty && candAuthors.nonEmpty) candAuthors
                      else if (candAuthors.isEmpty && old.authors.nonEmpty) old.authors
                      else if ((old.authors.size < candAuthors.size && (candNormAuthors.intersect(normAuthors).nonEmpty)) || (typeChange && candAuthors.size >= old.authors.size)) {
                        if (cand._type.toLowerCase == "game" && compatibleAuthors && differentNames && old.authors.forall(isRealName) && !cand.authors.forall(isRealName)) old.authors
                        else if (cand._type.nonEmpty && cand._type.toLowerCase != "game" && compatibleAuthors && differentNames && old.authors.forall(a => !isRealName(a)) && !candAuthors.forall(a => !isRealName(a))) old.authors
                        else if (compatibleAuthors && !differentNames && old.authors.size >= cand.authors.size) old.authors
                        else cand.authors
                      } else if (typeChange && compatibleAuthors && differentNames && (cand.authors.size >= old.authors.size || old.authors.exists(isRealName) != candAuthors.exists(isRealName))) cand.authors
                      else old.authors
                    val passesFilter = cachedDups.forall(h => _filter(cand.copy(hash = h)))
                    if (passesFilter && (!meta.isDefined || processAgain || compatibleAuthors || meta.get.authors.isEmpty)) {
                      if (meta.isDefined) {
                        debug(s"Overriding meta data entry ${meta.get} with ${cand}, score ${scores.getOrElse(hash, 0)} candscore ${candScore} typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${allowOverride}")
                        if (authors != old.authors) {
                          debug(s"Overriding authors for ${old} with ${authors}, cand: ${cand}, meta: ${meta}  typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${allowOverride}")
                        }
                      } else {
                        debug(s"Overriding meta data for md5 ${hash} with ${cand}" + s" typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${allowOverride}")
                      }
                      metasByHash(hash) = cand.copy(authors = authors, hash = hash)
                      applied = true
                    } else {
                      // candidate failed the main passesFilter check; try author-only override when it's safe
                      if (meta.isDefined && meta.get.authors.isEmpty && authors.nonEmpty) {
                        debug(s"Overriding authors for ${old} with ${authors}, cand: ${cand}, meta: ${meta}  typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${allowOverride} passesFilter: ${passesFilter}")
                        metasByHash(hash) = meta.get.copy(authors = authors)
                        applied = true
                      } else if (!meta.isDefined && authors.nonEmpty) {
                        debug(s"Overriding authors for md5 ${hash} with ${authors}, cand: ${cand}, meta: ${meta}  typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${allowOverride}" + s" passesFilter: ${passesFilter}")
                        metasByHash(hash) = MetaData(hash, authors, Buffer.empty, "", 0, "", "")
                        applied = true
                      } else {
                        trace(_ => s"Not overriding meta data for hash ${hash} meta ${meta} with ${cand}, score ${scores.getOrElse(hash, 0)} candscore ${candScore} candOk: ${candOk}, typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${allowOverride} passesFilter: ${passesFilter}")
                      }
                    /*
                    } else {
                      trace(_ => s"Not overriding meta data for hash ${hash} meta ${meta} with ${cand}, score ${scores.getOrElse(hash, 0)} candscore ${candScore} candOk: ${candOk}, typeChange: ${typeChange}, differentNames: ${differentNames}, compatibleAuthors: ${compatibleAuthors} allowOverride: ${allowOverride} passesFilter: ${passesFilter}")
                    */
                    }
                  } else {
                    // candidate-specific author-only override
                    if (meta.isDefined) {
                      if ((meta.get.authors.isEmpty && candAuthors.nonEmpty) || (
                          meta.get.authors.size < candAuthors.size &&
                          candNormAuthors.intersect(normAuthors).nonEmpty &&
                          ((candNormPublishers.isEmpty && meta.get.publishers.isEmpty) || candNormPublishers.exists(p => meta.get.publishers.map(normalizePublisher).exists(_.startsOrEndsWith(p)))) &&
                          ((cand.year == 0 && meta.get.year == 0) || cand.year == meta.get.year) &&
                          ((cand.album.isEmpty && meta.get.album.isEmpty) ||
                          (candNormAlbum == normalizeAlbum(meta.get))))
                      ) {
                        debug(s"Overriding authors for ${meta.get} with ${candAuthors.sorted}")
                        metasByHash(hash) = meta.get.copy(authors = candAuthors.sorted)
                        applied = true
                      } else {
                        trace(_ => s"Not overriding meta data for ${meta.get} with ${cand}, score ${scores.getOrElse(hash, 0)} candscore ${candScore} year ${meta.get.year} cand year ${cand.year} candOk: ${candOk} allowOverride: ${allowOverride}")
                      }
                    } else {
                      trace(_ => s"Not overriding meta data for md5 ${hash} with ${cand}, score ${scores.getOrElse(hash, 0)} candscore ${candScore} year  cand year ${cand.year} candOk: ${candOk} allowOverride: ${allowOverride}")
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
  var finalMetas = metasByHash.values.par.map(meta => {
    if (meta._platform.isEmpty) {
      val entries = songlengths.songlengthsByMd5(meta.hash)
      var platform = ""

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
        // Also used on Amiga
        if (!entries.exists(e => e.format == "Quartet ST" || e.format == "Rob Hubbard ST")) {
          platform = "Atari"
        }
      } else if (entries.exists(e =>
        e.format.toLowerCase.contains("soundtracker") ||
        e.format.toLowerCase.contains("noisetracker") ||
        e.format.contains("DigiBooster") ||
        e.format.contains("DIGI Booster") ||
        e.format.contains("OctaMED") ||
        e.format.contains("MED ") ||
        e.format.contains("Future Composer") ||
        e.format.contains("Face The Music") ||
        e.format.contains("Puma Tracker") ||
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
        platform = "Amiga"
      } else if (!entries.exists(e =>
        e.format.toLowerCase.contains("protracker") ||
        e.format.toLowerCase.contains("oktalyzer") ||
        e.format.endsWith("SID") ||
        e.format.contains("POKEYNoise") ||
        e.format.contains("Archimedes Tracker") ||
        e.format.contains("Coconizer") ||
        e.format.contains("Blade Packer") ||
        e.player == "hivelytracker" ||
        e.player == "ft2play"
      )) {
        if (entries.exists(_.player == "uade")) {
          platform = "Amiga"
        } else if (!entries.forall(_.player.isEmpty)) {
          platform = "PC"
        }
      }
      if (platform.nonEmpty) {
        debug(s"DEDUCED PLATFORM ${platform} for ${meta} based on formats: ${entries.map(_.format).distinct.mkString(", ")} players: ${entries.map(_.player).distinct.mkString(", ")}")
      }
      meta.copy(_platform = platform)
    } else meta
  }).seq.toBuffer

  for (pass <- 1 to 2) {

    val allmetas = (finalMetas ++ extraMetas)
      .filterNot(_.album.isEmpty)

    val metasWithAlbum = allmetas
      .filterNot(_.album.isEmpty)
      .groupBy(m => normalizeAlbum(m))

    val yearPublisher = allmetas
      .filterNot(_.year == 0)
      .filterNot(_.publishers.isEmpty)
      .par.flatMap(m => {
        m.publishers.map(normalizePublisher).flatMap { publisher =>
          Seq(
            (m.year - 1, publisher),
            (m.year, publisher),
            (m.year + 1, publisher)
          )
        }
      })
      .toSet

    val authorMetas = allmetas
      .filterNot(_.authors.isEmpty)
      .filterNot(m => m.year == 0 && m.publishers.isEmpty && m.album.isEmpty && m._type.isEmpty && m._platform.isEmpty)
      .par.flatMap(m => {
        m.authors.map(normalizeAuthor).flatMap { author =>
          Set(
            (author, m)
          )
        }
      })
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)


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
    
        val availableTypes = availableMetas.map(_._type).filterNot(_.isEmpty).toSet
        var metas = availableMetas
          .filterNot(_.publishers.isEmpty)
          //.filterNot(_.year == 0)
          .filter(m => meta.year == 0 || Math.abs(m.year - meta.year) <= 1)
          .filter(m => (m._type.toLowerCase.startsWith("game") && meta._type.toLowerCase.startsWith("game")) || (!m._type.toLowerCase.startsWith("game") && !meta._type.toLowerCase.startsWith("game")) || (meta._type.isEmpty && availableTypes.size <= 1))
          .filter(m => m.authors.isEmpty || meta.authors.isEmpty || meta.authors.map(normalizeAuthor).exists(a => m.authors.map(normalizeAuthor).exists(_ == a)))

        if (metas.isEmpty) {
          break()
        }

        if (metas.filter(m => filterByConstraints(m.copy(authors = Buffer.empty, hash = meta.hash)).isDefined).size >= 1) {
          metas = metas.filter(m => filterByConstraints(m.copy(authors = Buffer.empty, hash = meta.hash)).isDefined)
        }

        if (metas.filter(_.album.toLowerCase == meta.album.toLowerCase).size >= 1) {
          metas = metas.filter(_.album.toLowerCase == meta.album.toLowerCase)
        }

        if (meta.authors.nonEmpty && metas.filter(_.authors.map(normalizeAuthor).exists(a => meta.authors.map(normalizeAuthor).exists(_ == a))).size >= 1) {
          metas = metas.filter(_.authors.map(normalizeAuthor).exists(a => meta.authors.map(normalizeAuthor).exists(_ == a)))
        }

        if (meta.year != 0 && metas.filter(m => m.year != 0 && m.year == meta.year).size >= 1) {
          metas = metas.filter(m => m.year != 0 && m.year == meta.year)
        } else if (meta.year == 0 && metas.filter(_.year != 0).size >= 1) {
          metas = metas.filter(_.year != 0)
        }

        if (metas.filter(_._platform.toLowerCase == meta._platform.toLowerCase).size >= 1) {
          metas = metas.filter(_._platform.toLowerCase == meta._platform.toLowerCase)
        }

        val player = songlengths.songlengthsByMd5(meta.hash).head.player
        if (player == "uade" && metas.exists(_._platform == "Amiga") && metas.exists(m => m._platform.nonEmpty && m._platform != "Amiga")) {
          metas = metas.filter(m => m._platform.isEmpty || m._platform == "Amiga")
        }

        if (meta.publishers.nonEmpty && metas.filter(m => m.publishers.map(normalizePublisher).exists(p => meta.publishers.map(normalizePublisher).exists(_ == p))).size >= 1) {
          metas = metas.filter(m => m.publishers.map(normalizePublisher).exists(p => meta.publishers.map(normalizePublisher).exists(_ == p)))
        }

        var _metas = if (metas.exists(_.hash.nonEmpty)) metas.filter(_.hash.nonEmpty) else metas

        var cmp = _metas.sortBy(m => if (m.year > 0) m.year else 9999).filter(_.album.toLowerCase == meta.album.toLowerCase).headOption.getOrElse(_metas.head)
        val publishers = cmp.publishers.map(normalizePublisher).sorted.distinct

        lazy val availableMetas_ = availableMetas.filter(m => meta.authors.isEmpty || m.authors.map(normalizeAuthor).exists(a => meta.authors.map(normalizeAuthor).exists(_ == a)))
        lazy val yearOk = availableMetas_.forall(m => m.year == 0 || Math.abs(m.year - cmp.year) <= 1) ||
          availableMetas_.filter(_.hash.nonEmpty).forall(m => m.year == 0 || Math.abs(m.year - cmp.year) <= 1)

        lazy val yearPlatformTypeMatch = _metas
          .filter(m => meta._platform.isEmpty || m._platform.isEmpty || m._platform == meta._platform)
          .filter(m => meta._type.isEmpty || m._type.isEmpty || m._type == meta._type)
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
          lazy val publishersOk = _metas.forall(m => m.publishers.map(normalizePublisher).exists(p => publishers.exists(_.startsOrEndsWith(p))))
          if (publishersOk && yearPlatformTypeMatch &&
              publishers.exists(p => yearPublisher.contains((cmp.year, p)))
          ) {
            debug(s"Filling publishers and year for ${meta} - publishers ${meta.publishers.mkString(",")} -> ${cmp.publishers.mkString(",")}, year ${meta.year} -> ${cmp.year} source: ${cmp}")
            meta = meta.copy(publishers = cmp.publishers, year = cmp.year)
          } else if (publishersOk && yearPlatformTypeMatch && cmp.year == 0 && meta.year == 0 && cmp._platform == meta._platform) {
            debug(s"Filling publishers for ${meta} -  publishers ${meta.publishers.mkString(",")} -> ${cmp.publishers.mkString(",")} source: ${cmp}")
            meta = meta.copy(publishers = cmp.publishers)
          }

        } else if (meta.publishers.isEmpty && meta.year != 0 && yearPlatformTypeMatch && cmpOk) { 
          if (_metas.forall(m => m.publishers.map(normalizePublisher).exists(p => publishers.exists(_.startsOrEndsWith(p)))) &&
              publishers.exists(p => yearPublisher.contains((meta.year, p))) && yearOk
          ) {
            debug(s"Filling publishers for ${meta} -  publishers ${meta.publishers.mkString(",")} -> ${cmp.publishers.mkString(",")} source: ${cmp}")
            meta = meta.copy(publishers = cmp.publishers)
          }

        } else if (meta.publishers.nonEmpty && meta.year == 0 && cmp.year != 0 && yearPlatformTypeMatch && cmpOk) {
          if (yearOk &&
              meta.publishers.map(normalizePublisher).exists(p => publishers.exists(_.startsOrEndsWith(p))) &&
              publishers.exists(p => yearPublisher.contains((cmp.year, p)))
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
          .filter(m => meta._platform.isEmpty || m._platform.isEmpty || m._platform.toLowerCase == meta._platform.toLowerCase)

        if (availableMetas.isEmpty || availableMetas.forall(m => m.authors.isEmpty || m.publishers.isEmpty || m.year == 0)) {
          availableMetas = metasWithAlbum(key)
            .filterNot(_.hash == meta.hash)
        }

        val availableTypes = availableMetas.map(_._type).filterNot(_.isEmpty).toSet
        var metas = availableMetas
          .filter(m => (m._type.toLowerCase.startsWith("game") && meta._type.toLowerCase.startsWith("game")) || (!m._type.toLowerCase.startsWith("game") && !meta._type.toLowerCase.startsWith("game")) || (meta._type.isEmpty && availableTypes.size <= 1))
          .filter(m => m.publishers.map(normalizePublisher).exists(p => meta.publishers.map(normalizePublisher).exists(_ == p)))
          .filter(m => m.year == meta.year)

        if (metas.isEmpty) {
          break()
        }

        if (metas.filter(_.album.toLowerCase == meta.album.toLowerCase).size >= 1) {
          metas = metas.filter(_.album.toLowerCase == meta.album.toLowerCase)
        }

        if (metas.filter(_._platform.toLowerCase == meta._platform.toLowerCase).size >= 1) {
          metas = metas.filter(_._platform.toLowerCase == meta._platform.toLowerCase)
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
        lazy val headAuthors = metas_.head.authors.flatMap(a => getAuthorVariants(a, knownAuthors)).map(normalizeAuthor)
        if (metas_.size > 1 && (normAlbum.contains("megademo") || meta._type.toLowerCase == "musicdisk" || metas_.exists(_._type.toLowerCase == "musicdisk") ||
        !metas_.forall(m => {
          val normAuthors2 = m.authors.flatMap(a => getAuthorVariants(a, knownAuthors)).map(normalizeAuthor)
          normAuthors2.intersect(headAuthors).nonEmpty
        }))) {
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

        val cmp =
          authenticCMPsWithAlbum.filter(_.hash.nonEmpty).headOption
          .orElse(authenticCMPsWithAlbum.headOption)
          .orElse(authenticCMPs.filter(_.hash.nonEmpty).headOption)
          .orElse(authenticCMPs.headOption)
          .orElse(metas.filter(_.hash.nonEmpty).filter(a => normalizeAlbum(a) == normAlbum).headOption)
          .orElse(metas.filter(a => normalizeAlbum(a) == normAlbum).headOption)
          .orElse(metas.filter(_.hash.nonEmpty).headOption)
          .getOrElse(metas.head)

        if (!haveCompatibleAuthors(metas.map(_.authors) :+ cmp.authors, knownAuthors)) {
          break()
        }
        debug(s"Filling authors for key: ${key}, meta: ${meta}, source: ${cmp}, authenticCMPs: ${authenticCMPs.mkString(" | ")}, authenticCMPsWithAlbum: ${authenticCMPsWithAlbum.mkString(" | ")}, metas: ${metas.mkString(" | ")}")
      }
      meta
    ).toBuffer.sortBy(_.hash).distinct
  }

  finalMetas.par.map(m => {
    val lcalbum = m.album.toLowerCase
    var updated = m
    if (m._type.toLowerCase == "cracktro" && m.album.nonEmpty &&
       !m.album.matches(".* \\+[0-9]+$") &&
       !m.album.matches(".* \\(\\+[0-9]+\\)$") &&
       !m.album.matches(".* [0-9]+%$") &&
       !lcalbum.contains(" 100% ") &&
       !lcalbum.contains(" keygen ") &&
       !lcalbum.endsWith(" ++") &&
       !lcalbum.endsWith(" intro") &&
       !lcalbum.endsWith(" trainer") &&
       !lcalbum.endsWith("-trainer") &&
       !lcalbum.endsWith(" import") &&
       !lcalbum.endsWith(" pal/ntsc selector") &&
       !lcalbum.endsWith(" cd-rip") &&
       !lcalbum.endsWith("aga fix") &&
       !lcalbum.endsWith("hd fix") &&
       !lcalbum.endsWith("hd install") &&
       !lcalbum.endsWith(" pal fixed") &&
       !lcalbum.endsWith(" one filed") &&
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
      var nonpreviewmetas = authenticAuthorMetas.getOrElse(m.authors.map(normalizeAuthor).sorted.distinct, Set.empty).filter(m2 => m2.album != m.album && normAlbum == normalizeAlbum(m2) && !isPreview(m2.album.toLowerCase) && (m.year == 0 || m2.year == 0 || m2.year <= m.year) && (normPublishers.isEmpty || m2.publishers.isEmpty || m2.publishers.map(normalizePublisher).intersect(normPublishers).nonEmpty))
      if (nonpreviewmetas.filter(_.hash.nonEmpty).size >= 1) {
        nonpreviewmetas = nonpreviewmetas.filter(_.hash.nonEmpty)
      }
      val nonpreviewmeta = nonpreviewmetas.headOption
      lazy val audioHashes1 = audio.audioHashesByMd5(m.hash).toSet
      lazy val audioHashes2 = nonpreviewmetas.flatMap(m => audio.audioHashesByMd5.getOrElse(m.hash, Set.empty))
      if (nonpreviewmeta.isDefined && (audioHashes2.isEmpty || audioHashes2.intersect(audioHashes1).nonEmpty)) {
        updated = m.copy(album = nonpreviewmeta.get.album)
        debug(s"Stripped preview/demo from album name for ${m.hash} - ${m.album} -> ${updated.album}, non-preview meta: ${nonpreviewmeta.get}")
      // XXX
      } else if (m.album == "World Of Commodore 92 Preview") {
        updated = m.copy(album = "World of Commodore")
      }
    }
    if (m._type.toLowerCase == "game" && m.authors.exists(a => !isRealName(a)) && (m.publishers.isEmpty || !m.publishers.forall(sceneGroups.contains))) {
      val realNameAuthors = updated.authors.map(a => if (isRealName(a)) a else getAuthorVariants(a, knownAuthors).find(isRealName).getOrElse(a)).sorted.distinct
      if (realNameAuthors != updated.authors) {
        debug(s"Replacing non-real name authors with real names for ${updated} -> ${realNameAuthors.mkString(", ")}")
        updated = updated.copy(authors = realNameAuthors)
      }
    }
    val authors = updated.authors.map(a => unnormalizedAuthors.getOrElse(a, a)).sorted.distinct
    if (authors != updated.authors) {
      debug(s"Unnormalizing authors for ${updated} -> ${authors.mkString(", ")}")
      updated = updated.copy(authors = authors)
    }
    if (m._type.nonEmpty && m._type.toLowerCase != "game" && m.authors.size == 1 && isRealName(m.authors.head)) {
      val aliases = getAuthorVariants(m.authors.head, knownAuthors).filter(a => !isRealName(a))
      boundary {
        for (alias <- aliases if alias != m.authors.head &&
             // XXX
             !m.authors.head.startsWith("Øistein") &&
             !m.authors.head.startsWith("Øystein") &&
             !m.authors.head.endsWith("Hülsbeck") &&
             m.authors.head != "Vincent Voois"
        ) {
          val metas = authenticAuthorMetas.getOrElse(Buffer(normalizeAuthor(alias)).sorted.distinct, Set.empty)
          if (metas.exists(m2 => m2.authors.head == alias && (m2.hash == m.hash || normalizeAlbum(m2) == normalizeAlbum(m)))) {
            debug(s"Replacing real name authors with non-real names for ${updated} -> ${alias}")
            updated = updated.copy(authors = Buffer(alias))
            break()
          }
        }
      }
    }
    // XXX
    if (updated.authors.contains("Chris Huelsbeck")) {
      updated = updated.copy(authors = updated.authors.map(_.replace("Chris Huelsbeck", "Chris Hülsbeck")))
    }
    updated
  })
  .seq.toBuffer
}
