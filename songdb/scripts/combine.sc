// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0
//> using dep com.ibm.icu:icu4j:78.1

import java.util.concurrent.ConcurrentHashMap
import java.util.regex.Pattern
import scala.collection.Map
import scala.collection.Set
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSet
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala
import scala.util.boundary, boundary.break
import com.ibm.icu.text.Transliterator

import audio._
import chromaprint._
import convert._
import demozoo._
import exodos._
import songlengths._
import tosec._
import whdload._
import wikipedia._

val transliteratorThreadLocal = new ThreadLocal[Transliterator] {
  override def initialValue(): Transliterator = 
    Transliterator.getInstance("NFD; [:Nonspacing Mark:] Remove; NFC; Latin-ASCII")
}

def debug(msg: String): Unit = {
  System.err.println(s"DEBUG: $msg")
}

extension (s: String)
  def startsOrEndsWith(s2: String, minSLength: Int = 4, minELength: Int = 7): Boolean =
    s.nonEmpty && s2.nonEmpty && (s == s2 ||
      (s.length >= minSLength && s2.length >= minSLength && (
        s.startsWith(s2) || s2.startsWith(s))) ||
      (s.length >= minELength && s2.length >= minELength && (
        s.endsWith(s2) || s2.endsWith(s))))

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

val normalizePattern = Pattern.compile("[^A-Za-z0-9]")
val normalizeCache = new ConcurrentHashMap[String, String]()
def normalize(s: String): String = {
  if (s.isEmpty) s
  else normalizeCache.computeIfAbsent(s, s => {
    val lower = s.toLowerCase
    var transliterated = transliteratorThreadLocal.get().transliterate(lower)
    if (transliterated.replace(" ", "").trim.length >= 7) {
      val head = transliterated.trim.split(" ")(0)
      if (head.length >= 4) transliterated = head
    }
    normalizePattern.matcher(transliterated).replaceAll("").trim
  })
}

val normalizeAlbumPatterns = Seq(
  ("\\(.*\\)",""),
  (" PC$",""),
  (" [vV][0-9]+(\\.[0-9]+)*\\b",""), // TODO [vV] optional
  (" #(.*)$"," $1"),
  (" 0([1-9][0-9])$"," $1"),
  (" 00([0-9])$"," $1"),
  (" 0([0-9])$"," $1"),
  (" 0$",""),
  (" 1$",""),
  (" [Ii]$",""),
  (" [Ii][Ii]$"," 2"),
  (" [Ii][Ii][Ii]$"," 3"),
  (" [Ii][Vv]$"," 4"),
  (" [Vv]$"," 5"),
  (" [Vv][Ii]$"," 6"),
  (" [Vv][Ii][Ii]$"," 7"),
  (" [Vv][Ii][Ii][Ii]$"," 8"),
  (" [Ii][Xx]$"," 9")
).map { case (pattern, replacement) => (Pattern.compile(pattern), replacement) }
val normalizePattern2 = Pattern.compile("[^A-Za-z0-9\\.]")
val normalizeAlbumCache = new ConcurrentHashMap[String, String]()
def normalizeAlbum(m: MetaData): String = normalizeAlbum(m._type, m.album, m.publishers)
def normalizeAlbum(_type: String, album: String, publishers: Buffer[String]): String = {
  if (album.isEmpty) ""
  else {
    var a = album
    if (_type.toLowerCase == "cracktro")
      a = a.trim + " [cracktro]"
    publishers.foreach(p =>
      a = a.replaceAll(s"^${Pattern.quote(p)} ", "")
    )
    normalizeAlbumCache.computeIfAbsent(a, a => {
      val normalized = normalizeAlbumPatterns.foldLeft(a) { case (acc, (pattern, replacement)) =>
        pattern.matcher(acc).replaceAll(replacement)
      }.toLowerCase

      val transliterated = transliteratorThreadLocal.get().transliterate(normalized)
      normalizePattern2.matcher(transliterated).replaceAll("").trim
    })
  }
}

def pickMostCommonPublishers(metas: Set[MetaData]): Buffer[String] = {
  val grouped = metas.filter(_.publishers.nonEmpty).groupBy(_.publishers.sorted)
  if (grouped.nonEmpty) {
    grouped.seq.view.mapValues(_.size).maxBy(_._2)._1
  } else {
    Buffer.empty
  }
}

def pickMostCommonYear(metas: Set[MetaData]): Int = {
  val years = metas.filter(_.year != 0).map(_.year).seq
  if (years.nonEmpty) {
    val grouped = years.groupBy(identity)
    val mostCommon = grouped.view.mapValues(_.size).maxBy(_._2)._1
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

  val extraMetas = Set.empty[MetaData] ++ (
    (amp ++ modland ++ unexotica ++ demozoo ++ oldexotica ++ wantedteam ++ modsanthology ++ fujiology2).map(_.copy(hash = ""))
    ++
    tosecMetas ++ whdloadMetas ++ demozooMetas ++ exodosMetas ++ wikipediaMetas ++ tosecmusic.map(_.copy(hash = ""))
  )
    .filterNot(m => (m._type == "Game" && m._platform == "PC" && (m.year > 0 && m.year < 1991)))
    .filterNot(m => (m._platform == "PC" && (m.year > 0 && m.year < 1990)))
    .filterNot(m => (m._platform == "Atari" && (m.year > 0 && m.year < 1989)))
    .par
    .flatMap(expandArticleVariants)
    .seq

  val demozoog = demozoo.groupBy(_.hash).par.mapValues(_.head).seq
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
  //val tosecmusicg = tosecmusic.groupBy(_.hash).par.mapValues(_.head).seq // too unreliable

  var metas = ParSet.empty[MetaData]
  var metasByHash = new ConcurrentHashMap[String, MetaData]().asScala

  // authors: AMP > Demozoo > Modland > UnExotica > OldExotica > WantedTeam > ModsAnthology > Fujiology
  val authorSources = Seq(
    ampg,
    demozoog,
    modlandg,
    unexoticag,
    oldexoticag,
    wantedteamg,
    modsanthologyg,
    fujiologyg,
    // tosecmusicg, // too unreliable
  )
  // album/publishers/year: UnExotica > Demozoo > OldExotica > WantedTeam > AMP > Modland > ModsAnthology > Fujiology
  val allMetaSources = Seq(unexoticag, demozoog, oldexoticag, wantedteamg, ampg, modlandg, modsanthologyg, fujiologyg)

  // precompute per-audioTag data that doesn't change across passes
  val audioTagData = audio.audioByAudioTags.map { case (audioTag, entries) =>
    val entriesByHash = entries.groupBy(_.md5).view.mapValues(_.distinctBy(_.normalizedSubsong)).toMap
    val hashes = entries.map(_.md5).distinct.sorted
    (audioTag, entries, entriesByHash, hashes)
  }
  // group audioTags into connected components by shared md5s
  // audioTags in different components touch disjoint md5 sets and can run in parallel
  val audioTagKeys = audioTagData.map(_._1).toArray
  val parent = mutable.Map[String, String]()
  def find(x: String): String = {
    var r = x
    while (parent.getOrElse(r, r) != r) r = parent.getOrElse(r, r)
    var c = x
    while (c != r) { val n = parent.getOrElse(c, c); parent(c) = r; c = n }
    r
  }
  def union(a: String, b: String): Unit = { parent(find(a)) = find(b) }
  // build mapping: md5 -> first audioTag that uses it, then union subsequent audioTags
  val md5FirstTag = mutable.Map[String, String]()
  for ((audioTag, _, _, hashes) <- audioTagData; h <- hashes) {
    md5FirstTag.get(h) match {
      case Some(first) => union(audioTag, first)
      case None => md5FirstTag(h) = audioTag
    }
  }
  val audioTagDataMap = audioTagData.map(t => (t._1, t)).toMap
  val components = audioTagKeys.groupBy(find).values.map(_.map(audioTagDataMap).toSeq).toSeq

  // precompute duplicate relationships per audioTag: for each hash, all hashes that are audio-duplicates of it
  // this is constant across all passes since it depends only on audio data
  val duplicatesForTag = components.par.flatMap { component =>
    component.map { case (audioTag, _, entriesByHash, hashes) =>
      val dupMap = hashes.map { cmpHash =>
        val cmpAudioEntries = entriesByHash(cmpHash)
        val dups = hashes.par.filter { hash =>
          val audioEntries = entriesByHash(hash)
          assert(audioEntries.size == cmpAudioEntries.size)
          var duplicate = true
          var i = 0
          while (i < audioEntries.size && duplicate) {
            if (cmpAudioEntries(i).audioMd5 == audioEntries(i).audioMd5) {
              // duplicate = true
            } else if (cmpAudioEntries(i).audioChromaprint != audioEntries(i).audioChromaprint) {
              val threshold = if (audioEntries(i).audioBytes > 2 * 11025 * 12) 0.9 else 0.99
              val similarity = chromaSimilarity(cmpAudioEntries(i).audioChromaprint, audioEntries(i).audioChromaprint)
              if (similarity < threshold) duplicate = false
            } else if (cmpAudioEntries(i).audioHash != audioEntries(i).audioHash) {
              duplicate = false
            }
            i += 1
          }
          duplicate
        }.seq.toSet
        (cmpHash, dups)
      }.toMap
      (audioTag, dupMap)
    }
  }.seq.toMap

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
    metas = hashes.par.map { hash =>
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

        if (existing.isDefined && (existing.get.publishers.nonEmpty || existing.get.album.nonEmpty || existing.get.year != 0) && (!picked.isDefined || pScore < eScore || (pScore == eScore && (picked.get.year == 0 || picked.get.year == existing.get.year || picked.get.year > existing.get.year))))
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

      debug(s"initial pick for $hash -> authors: ${authors}, album: '${album}', publishers: ${publishers}, year: ${year}, type: ${_type}, platform: ${_platform}")
      MetaData(hash, authors, publishers, album, year, _type, _platform)
    }
  
    // Duplicate entries with "The X", "An X", "A X" to include both versions
    val allmetas = metas.par.flatMap(expandArticleVariants).seq ++ extraMetas

    val yearAlbumPublishers = allmetas
      .filterNot(_.album.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.year, normalizeAlbum(m), m.publishers.map(normalize).sorted.distinct))
  
    val yearAlbum = allmetas
      .filterNot(_.album.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.year, normalizeAlbum(m)))

    val albumPublishers = allmetas
      .filterNot(_.album.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .map(m => (normalizeAlbum(m), m.publishers.map(normalize).sorted.distinct))

    val authorsAlbumPublishers = allmetas
      .filterNot(_.authors.isEmpty)
      .filterNot(_.album.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .map(m => (m.authors.map(normalize).sorted.distinct, normalizeAlbum(m), m.publishers.map(normalize).sorted.distinct))
  
    val authorsAlbumYear = allmetas
      .filterNot(_.authors.isEmpty)
      .filterNot(_.album.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.authors.map(normalize).sorted.distinct, normalizeAlbum(m), m.year))

    val authorsYearPublishers = allmetas
      .filterNot(_.authors.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.authors.map(normalize).sorted.distinct, m.year, m.publishers.map(normalize).sorted.distinct))

    val authorsYearNoAlbum = allmetas
      .filter(_.album.isEmpty)
      .filterNot(_.authors.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.authors.map(normalize).sorted.distinct, m.year))

    val authorsPublishersNoAlbum = allmetas
      .filter(_.album.isEmpty)
      .filterNot(_.authors.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .map(m => (m.authors.map(normalize).sorted.distinct, m.publishers.map(normalize).sorted.distinct))

    metas = metas.par.map { meta =>
      val hash = meta.hash
      val authors = meta.authors
      var album = meta.album
      var publishers = meta.publishers
      var year = meta.year
      var _type = meta._type
      var _platform = meta._platform

      def pickYearAlbumWithPublishers(m: Option[MetaData]) = {
        if (year == 0 && album.isEmpty && m.isDefined && m.get.year != 0 && m.get.album.nonEmpty) {
          if (publishers.nonEmpty && yearAlbumPublishers.exists(e => e._1 == m.get.year &&
              e._2 == normalizeAlbum(m.get) &&
              e._3.exists(p => publishers.map(normalize).exists(_.startsOrEndsWith(p))))
          ) {
            debug(s"pickYearAlbumWithPublishers: $hash -> ${m.get.year} + ${m.get.album}")
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
            debug(s"pickYearAlbumWithoutPublishers: $hash -> ${m.get.year} + ${m.get.album}")
            year = m.get.year
            album = m.get.album
          }
        }
      }

      def pickAlbumPublishersWithYear(m: Option[MetaData]) = {
        if (album.isEmpty && publishers.isEmpty && m.isDefined && m.get.album.nonEmpty && m.get.publishers.nonEmpty) {
          if (year != 0 && yearAlbumPublishers.exists(e => e._1 == year &&
              e._2 == normalizeAlbum(m.get) &&
              e._3.exists(p => m.get.publishers.map(normalize).exists(_.startsOrEndsWith(p))))
          ) {
            debug(s"pickAlbumPublishersWithYear: $hash -> ${m.get.album} + ${m.get.publishers}")
            album = m.get.album
            publishers = m.get.publishers
          }
        }
      }

      def pickAlbumPublishersWithoutYear(m: Option[MetaData]) = {
        if (album.isEmpty && publishers.isEmpty && m.isDefined && m.get.album.nonEmpty && m.get.publishers.nonEmpty) {
          if (year == 0 && albumPublishers.exists(e => e._1 == normalizeAlbum(m.get) &&
              e._2.exists(p => m.get.publishers.map(normalize).exists(_.startsOrEndsWith(p))))
           ) {
            debug(s"pickAlbumPublishersWithoutYear: $hash -> ${m.get.album} + ${m.get.publishers}")
            album = m.get.album
            publishers = m.get.publishers
          }
        }
      }

      def pickAlbumWithPublishersAndYear(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty) {
          if (year != 0 && publishers.nonEmpty && yearAlbumPublishers.exists(e => e._1 == year &&
              e._2 == normalizeAlbum(m.get) &&
              e._3.exists(p => publishers.map(normalize).exists(_.startsOrEndsWith(p))))
          ) {
            debug(s"pickAlbumWithPublishersAndYear: $hash -> ${m.get.album}")
            album = m.get.album
          }
        }
      }

      def pickAlbumWithPublishers(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty) {
          if (year == 0 && publishers.nonEmpty && albumPublishers.exists(e =>
              e._1 == normalizeAlbum(m.get) &&
              e._2.exists(p => publishers.map(normalize).exists(_.startsOrEndsWith(p))))
          ) {
            debug(s"pickAlbumWithPublishers: $hash -> ${m.get.album}")
            album = m.get.album
          }
        }
      }

      def pickAlbumWithYear(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty) {
          if (publishers.isEmpty && year != 0 && yearAlbum.exists(e => e._1 == year &&
              e._2 == normalizeAlbum(m.get))
           ) {
            debug(s"pickAlbumWithYear: $hash -> ${m.get.album}")
            album = m.get.album
          }
        }
      }

      def pickPublishersWithAlbumAndYear(m: Option[MetaData]): Unit = {
        if (publishers.isEmpty && m.isDefined && m.get.publishers.nonEmpty) {
          if (year != 0 && album.nonEmpty && yearAlbumPublishers.exists(e => e._1 == year &&
              e._2 == normalizeAlbum(_type, album, m.get.publishers) &&
              e._3.exists(p => m.get.publishers.map(normalize).exists(_.startsOrEndsWith(p))))
          ) {
            debug(s"pickPublishersWithAlbumAndYear: $hash -> ${m.get.publishers}")
            publishers = m.get.publishers
          }
        }
      }

      def pickPublishersWithAlbum(m: Option[MetaData]): Unit = {
        if (publishers.isEmpty && m.isDefined && m.get.publishers.nonEmpty) {
          if (year == 0 && album.nonEmpty && albumPublishers.exists(e =>
              e._1 == normalizeAlbum(_type, album, m.get.publishers) &&
              e._2.exists(p => m.get.publishers.map(normalize).exists(_.startsOrEndsWith(p))))
          ) {
            debug(s"pickPublishersWithAlbum: $hash -> ${m.get.publishers}")
            publishers = m.get.publishers
          }
        }
      }

      def pickYearWithAlbumAndPublishers(m: Option[MetaData]) = {
        if (year == 0 && m.isDefined && m.get.year != 0) {
          if (album.nonEmpty && publishers.nonEmpty && yearAlbumPublishers.exists(e => e._1 == m.get.year &&
              e._2 == normalizeAlbum(_type, album, publishers) &&
              e._3.exists(p => publishers.map(normalize).exists(_.startsOrEndsWith(p))))
          ) {
            debug(s"pickYearWithAlbumAndPublishers: $hash -> ${m.get.year}")
            year = m.get.year
          }
        }
      }

      def pickYearWithAlbum(m: Option[MetaData]) = {
        if (year == 0 && m.isDefined && m.get.year != 0) {
          if (publishers.isEmpty && album.nonEmpty && yearAlbum.exists(e => e._1 == m.get.year &&
              e._2 == (normalizeAlbum(_type, album, Buffer.empty)))
           ) {
            debug(s"pickYearWithAlbum: $hash -> ${m.get.year}")
            year = m.get.year
          }
        }
      }

      def pickAlbumWithAuthorsYearPublishers(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty) {
          if (authors.nonEmpty && year != 0 && publishers.nonEmpty &&
              authorsYearPublishers.exists(e =>
                e._1.exists(a => authors.map(normalize).exists(_.startsOrEndsWith(a))) &&
                e._2 == year &&
                e._3.exists(p => publishers.map(normalize).exists(_.startsOrEndsWith(p)))
              ) &&
              authorsAlbumPublishers.exists(e =>
                e._1.exists(a => authors.map(normalize).exists(_.startsOrEndsWith(a))) &&
                e._2 == normalizeAlbum(m.get) &&
                e._3.exists(p => publishers.map(normalize).exists(_.startsOrEndsWith(p)))
              ) &&
              authorsAlbumYear.exists(e =>
                e._1.exists(a => authors.map(normalize).exists(_.startsOrEndsWith(a))) &&
                e._2 == normalizeAlbum(m.get) &&
                e._3 == year)
          ) {
            debug(s"pickAlbumWithAuthorsYearPublishers: $hash -> ${m.get.album}")
            album = m.get.album
          }
        }
      }

      def pickAlbumWithAuthorsYear(m: Option[MetaData]) = {
        if (album.isEmpty && publishers.isEmpty && m.isDefined && m.get.album.nonEmpty && m.get.  publishers.isEmpty) {
          if (authors.nonEmpty && year == m.get.year && authorsAlbumYear.exists(e =>
              e._1.exists(a => authors.map(normalize).exists(_.startsOrEndsWith(a))) &&
              e._2 == normalizeAlbum(m.get) &&
              e._3 == year)
          ) {
            debug(s"pickAlbumWithAuthorsYear: $hash -> ${m.get.album}")
            album = m.get.album
          }
        }
      }

      def pickAlbumWithAuthorsPublishers(m: Option[MetaData]) = {
        if (album.isEmpty && m.isDefined && m.get.album.nonEmpty && m.get.year == year) {
          if (authors.nonEmpty && publishers.nonEmpty && authorsAlbumPublishers.exists(e =>
              e._1.exists(a => authors.map(normalize).exists(_.startsOrEndsWith(a))) &&
              e._2 == normalizeAlbum(m.get) &&
              e._3.exists(p => publishers.map(normalize).exists(_.startsOrEndsWith(p))))
          ) {
            debug(s"pickAlbumWithAuthorsPublishers: $hash -> ${m.get.album}")
            album = m.get.album
          }
        }
      }

      def pickYearPublishersWithoutAlbum(m: Option[MetaData]) = {
        if (year == 0 && publishers.isEmpty && album.isEmpty && m.isDefined && m.get.year != 0 && m.get.publishers.nonEmpty) {
          if (authors.nonEmpty && m.get.album.isEmpty &&
              authorsYearPublishers.exists(e =>
                e._1.exists(a => authors.map(normalize).exists(_.startsOrEndsWith(a))) &&
                e._2 == m.get.year &&
                e._3.exists(p => m.get.publishers.map(normalize).exists(_.startsOrEndsWith(p)))
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
                e._1.exists(a => authors.map(normalize).exists(_.startsOrEndsWith(a))) &&
                e._2 == m.get.year)
          ) {
            debug(s"pickYearWithoutAlbum: $hash -> ${m.get.year}")
            year = m.get.year
          }
        }
      }

      def pickPublishersWithoutAlbum(m: Option[MetaData]) = {
        if (publishers.isEmpty && album.isEmpty && m.isDefined && m.get.publishers.nonEmpty && m.get.year == year) {
          if (authors.nonEmpty && m.get.album.isEmpty &&
              authorsPublishersNoAlbum.exists(e =>
                e._1.exists(a => authors.map(normalize).exists(_.startsOrEndsWith(a))) &&
                e._2.exists(p => m.get.publishers.map(normalize).exists(_.startsOrEndsWith(p)))
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

      MetaData(meta.hash, meta.authors, publishers, album, year, _type, _platform)
    }
  
    for (pass <- 1 to 2) {
      def debug(msg: String): Unit = {
        System.err.println(s"DEBUG ($pass): $msg")
      }
      // find metas which have common author(s) + album, add publishers and year if missing
      val metasByAuthorAlbumWithPublisherOrYear = (metas.par.flatMap(expandArticleVariants).seq ++ extraMetas)
        .filterNot(_.authors.isEmpty)
        .filterNot(_.album.isEmpty)
        .filterNot(m => m.publishers.isEmpty && m.year == 0)
        .groupBy(m => (
          m.authors.map(normalize),
          normalizeAlbum(m)
        ))
        .toSeq
        .flatMap { case (key, metas) =>
          key._1.map(a => (a, key._2) -> metas)
        }
        .groupBy(_._1)
        .mapValues(_.flatMap(_._2).toSet)

      metas = metas.par.map(m =>
        if (m.authors.isEmpty || m.album.isEmpty || (m.publishers.nonEmpty && m.year != 0)) m
        else {
          val keys = m.authors.map(a => 
            (normalize(a),
             normalizeAlbum(m))
          )
          trace(_ => s"(1) ${m.hash} keys: ${keys}")
          val key = keys.find(metasByAuthorAlbumWithPublisherOrYear.contains(_))
          if (key.isDefined) {
            val metas = metasByAuthorAlbumWithPublisherOrYear(key.get)
            var publishers = if (m.publishers.isEmpty) pickMostCommonPublishers(metas) else m.publishers
            if (!metas.forall(m => m.publishers.isEmpty
              || m.publishers.map(normalize).exists(p => publishers.map(normalize).exists(_.startsOrEndsWith(p))))
            ) {
              warn(s"(1) publishers differ for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.publishers.mkString(",")} != ${metas.flatMap(_.publishers).mkString(",")}")
            }
            var year = if (m.year == 0) pickMostCommonYear(metas) else m.year
            if (!metas.forall(m => m.year == 0 || m.year == year)) {
              warn(s"(1) year differs for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.year} != ${metas.map(_.year).mkString(",")}")
              year = m.year
            }
            debug(s"(1) overriding metadata for ${m.hash} - ${m.authors.mkString(",")} - ${m.album}: publishers ${m.publishers.mkString(",")} -> ${publishers.mkString(",")}, year ${m.year} -> ${year}")
            trace(_ => s"(1) ${m.hash} metas: ${metas.seq} key: ${key}")
            m.copy(publishers = publishers, year = year)
          } else {
            m
          }
        }
      )
  
      // find metas which have common publisher(s) + album, add year if missing
      val metasByPublisherAlbumWithYear = (metas.par.flatMap(expandArticleVariants).seq ++ extraMetas)
        .filterNot(_.publishers.isEmpty)
        .filterNot(_.album.isEmpty)
        .filter(_.year != 0)
        .groupBy(m => (
          m.publishers.map(normalize),
          normalizeAlbum(m)
        ))
        .toSeq
        .flatMap { case (key, metas) =>
          key._1.map(p => (p, key._2) -> metas)
        }
        .groupBy(_._1)
        .mapValues(_.flatMap(_._2).toSet)

      metas = metas.par.map(m =>
        if (m.album.isEmpty || m.publishers.isEmpty || m.year != 0) m
        else {
          val keys = m.publishers.map(p => 
            (normalize(p),
            normalizeAlbum(m))
          )
          trace(_ => s"(2) ${m.hash} keys: ${keys}")
          val key = keys.find(metasByPublisherAlbumWithYear.contains(_))
          if (key.isDefined) {
            val metas = metasByPublisherAlbumWithYear(key.get)
            var year = if (m.year == 0) pickMostCommonYear(metas) else m.year
            if (!metas.forall(m => m.year == 0 || m.year == year)) {
              warn(s"(2) year differs for ${m.hash} - ${m.album} - ${m.publishers.mkString(",")} - ${m.year} != ${metas.map(_.year).mkString(",")}")
              m
            } else {
              debug(s"(2) overriding year for ${m.hash} - ${m.album} - ${m.publishers. mkString(",")}: year ${m.year} -> ${year}")
              trace(_ => s"(2) ${m.hash} metas: ${metas.seq} key: ${key}")
              m.copy(year = year)
            }
          } else {
            m
          }
        }
      )

      // if meta author is missing, compare to other metas
      // and when there is only 1 album with same non-empty name and only 1 distinct author(s) for that album and publisher matches (or is missing in the original meta)
      // -> add author, publisher and year
      val metasByAlbumWithAuthorPublisherOrYear = (metas.par.flatMap(expandArticleVariants).seq ++ extraMetas)
        .filterNot(_.album.isEmpty)
        .filterNot(m => m.publishers.isEmpty && m.year == 0)
        .groupBy(m => normalizeAlbum(m))

      metas = metas.par.map(m => {
        if (m.authors.nonEmpty || m.album.isEmpty || (m.publishers.nonEmpty && m.year != 0) ||
           (m.album.nonEmpty && m.authors.isEmpty && m.publishers.isEmpty && m.year == 0)) m
        else {
          val key = normalizeAlbum(m)
          trace(_ => s"(3) ${m.hash} key: ${key}")
          val metas = metasByAlbumWithAuthorPublisherOrYear.get(key)
          if (metas.isDefined && metas.get.size >= 1) {
            val authors = {
              val grouped = metas.get.groupBy(_.authors.sorted)
              grouped.seq.view.mapValues(_.size).maxBy(_._2)._1
            }
            if (metas.get.forall(_.authors.map(normalize).exists(a => authors.map(normalize).exists(_.startsOrEndsWith(a))))) {
              var publishers = if (m.publishers.isEmpty) pickMostCommonPublishers(metas.get) else m.publishers
              if (!metas.get.forall(m => m.publishers.isEmpty
                  || m.publishers.map(normalize).exists(p => publishers.map(normalize).exists(_.startsOrEndsWith(p))))
              ) {
                warn(s"(3) publishers differ for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.publishers.mkString(",")} != ${metas.get.flatMap(_.publishers).mkString(",")}")
              }
              var year = if (m.year == 0) pickMostCommonYear(metas.get) else m.year
              if (!metas.get.forall(m => m.year == 0 || m.year == year)) {
                warn(s"(3) year differs for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.year} != ${metas.get.map(_.year).mkString(",")}")
              }
              debug(s"(3) overriding metadata for ${m.hash} - ${m.authors.mkString(",")} - ${m.album}: publishers ${m.publishers.mkString(",")} -> ${publishers.mkString(",")}, year ${m.year} -> ${year}")
              trace(_ => s"(3) ${m.hash} metas: ${metas.get.seq} key: ${key}")
              m.copy(authors = authors, publishers = publishers, year = year)
            } else {
              m
            }
          } else {
            m
          }
        }
      })
    }

    // fill/update metadatas using audio fingerprints
    metasByHash = new ConcurrentHashMap[String, MetaData]().asScala
    metasByHash ++= metas.groupBy(_.hash).mapValues(_.head)
    // process twice to get metadata from "transitive" duplicates also (A matches B, B matches C, but A doesn't match C)
    val processAgain = pass == allMetaSources.size

    def processAudioTag(audioTag: String, entries: collection.Seq[AudioFingerprint], entriesByHash: collection.Map[String, collection.Seq[AudioFingerprint]], hashes: collection.Seq[String]): Unit = {
        trace(_ => s"Processing audio tag ${audioTag} with entries: ${entries.map(_.copy(audioChromaprint = "", audioHash = "", audioSimHash = "")).seq}")
        var remainingHashes = hashes
        var metas = remainingHashes.flatMap(h => metasByHash.get(h))
         // use all hashes for final pass
        if (processAgain) {
          metas ++= remainingHashes.filterNot(metasByHash.contains).map(h => MetaData(h, Buffer.empty, Buffer.empty, "", 0, "", ""))
        }
        trace(_ => s"Found ${metas.size} matching metas for audio tag ${audioTag}: ${metas.seq}")
        while (metas.nonEmpty && remainingHashes.nonEmpty) {
          var cmp = metas.head
          var anyMetadata = metas.exists(m => m.authors.nonEmpty || m.publishers.nonEmpty || m.album.nonEmpty || m.year != 0)
          if (anyMetadata && (!metas.forall(_ == metas.head) || remainingHashes.size != metas.size)) {
            trace(_ => s"Metas or hashes differ for audio tag ${audioTag}, comparing ${cmp.hash} with ${remainingHashes.mkString(", ")}")
            val cachedDups = duplicatesForTag(audioTag)(cmp.hash)
            val duplicateHashes = remainingHashes.filter(cachedDups.contains).toBuffer.sorted.distinct
            val duplicateMetas = duplicateHashes.flatMap(h => metasByHash.get(h)).distinct
            val anyMetadata = duplicateMetas.exists(m => m.authors.nonEmpty || m.publishers.nonEmpty || m.album.nonEmpty || m.year != 0)
            if (anyMetadata && duplicateHashes.size > 1) {
              trace(_ => s"Found ${duplicateHashes.size} duplicate hashes for audio tag ${audioTag}: ${duplicateHashes.mkString(", ")} with metas: ${duplicateMetas.mkString(" | ")}")
              // select best based on some ad hoc metadata heuristics
              val scores = duplicateMetas.map(e => (e.hash, e.publishers.size + (if (e.album.nonEmpty) 1 else 0) + (if (e.year > 0) 99 else 0))).toMap
              val bestscore = scores.maxBy(_._2)._2
              val bestmetas = duplicateMetas.filter(e => e.publishers.size + (if (e.album.nonEmpty) 1 else 0) + (if (e.year > 0) 99 else 0) == bestscore)
              val minyear = bestmetas.map(e => if (e.year > 0) e.year else 9999).min
              val byyear = bestmetas.filter(_.year == minyear)
              // pick majority publisher/album combination matching minyear
              def pickBySourcePriority(
                candidates: Iterable[MetaData]
              ): Option[MetaData] = {
                authorSources.view.flatMap { source =>
                  val matches = candidates.filter(c => source.get(c.hash).exists(src =>
                    (src.authors == c.authors || src.authors.map(normalize).exists(a => c.authors.map(normalize).exists(_.startsOrEndsWith(a)))) &&
                    (src.publishers == c.publishers || src.publishers.map(normalize).exists(p => c.publishers.map(normalize).exists(_.startsOrEndsWith(p)))) &&
                    normalizeAlbum(src) == normalizeAlbum(c) &&
                    src.year == c.year
                  ))
                  if (matches.nonEmpty) Some(matches.maxBy(_.authors.size))
                  else None
                }.headOption
              }

              var best = if (byyear.nonEmpty) {
                val grouped = byyear.groupBy(m => (m.publishers.map(normalize).sorted, normalizeAlbum(m)))
                val maxCount = grouped.view.mapValues(_.size).maxBy(_._2)._2
                val tiedGroups = grouped.filter(_._2.size == maxCount)
                val candidates = tiedGroups.values.flatten.toSeq
                val best = if (candidates.size > 1) {
                  pickBySourcePriority(candidates).getOrElse(candidates.maxBy(_.authors.size))
                } else {
                  candidates.maxBy(_.authors.size)
                }
                trace(_ => s"Best candidate by publisher/album majority for ${audioTag} with min year ${minyear} is ${best.hash} with publishers ${best.publishers.mkString(", ")} and album ${best.album} and year ${best.year} candidates: ${candidates.mkString(" | ")} grouped size: ${grouped.size}")
                best
              } else {
                // fallback: use majority voting on publisher/album for all bestmetas
                val grouped = bestmetas.groupBy(m => (m.publishers.map(normalize).sorted, normalizeAlbum(m)))
                if (grouped.size > 1) {
                  val maxCount = grouped.view.mapValues(_.size).maxBy(_._2)._2
                  val tiedGroups = grouped.filter(_._2.size == maxCount)
                  val candidates = tiedGroups.values.flatten.toSeq
                  val best = if (candidates.size > 1) {
                    pickBySourcePriority(candidates).getOrElse(candidates.maxBy(_.authors.size))
                  } else {
                    candidates.maxBy(_.authors.size)
                  }
                  trace(_ => s"Best candidate by publisher/album majority for ${audioTag} is ${best.hash} with publishers ${best.publishers.mkString(", ")} and album ${best.album} and year ${best.year} candidates: ${candidates.mkString(" | ")} grouped size: ${grouped.size}")
                  best
                } else {
                  // fallback: use majority voting on authors for all bestmetas
                  val grouped = bestmetas.groupBy(m => m.authors.map(normalize).sorted)
                  val best = if (grouped.size > 1) {
                    val maxCount = grouped.view.mapValues(_.size).maxBy(_._2)._2
                    val tiedGroups = grouped.filter(_._2.size == maxCount)
                    val candidates = tiedGroups.values.flatten.toSeq
                    if (candidates.size > 1) {
                      pickBySourcePriority(candidates).getOrElse(candidates.head)
                    } else {
                      candidates.head
                    }
                  } else {
                    bestmetas.maxBy(_.authors.size)
                  }
                  trace(_ => s"Best candidate by author majority for ${audioTag} is ${best.hash} with authors ${best.authors.mkString(", ")} and publishers ${best.publishers.mkString(", ")} and album ${best.album} and year ${best.year} candidates: ${bestmetas.mkString(" | ")} grouped size: ${grouped.size}")
                  best
                }
              }
              // use the most common authors as best
              if (best.authors.isEmpty) {
                val authorGroups = duplicateMetas.groupBy(_.authors.sorted).filter(_._1.nonEmpty)
                val bestAuthors = if (authorGroups.nonEmpty) {
                  val maxCount = authorGroups.view.mapValues(_.size).maxBy(_._2)._2
                  val tiedAuthors = authorGroups.filter(_._2.size == maxCount).keys.toSeq
                  // Pick first authors found in authorSources priority order
                  authorSources.view.flatMap { source =>
                    tiedAuthors.filter { authors =>
                      duplicateHashes.exists(hash =>
                        source.get(hash).exists(_.authors.sorted == authors)
                      )
                    }.headOption
                  }.headOption.getOrElse(tiedAuthors.head)
                } else {
                  Buffer.empty
                }
                best = best.copy(authors = bestAuthors)
              }
              debug(s"Combining ${duplicateHashes.mkString(", ")} to ${best.hash} with score ${bestscore} (${scores.map(e => s"${e._1}:${e._2}").mkString(", ")}) duplicate metas: ${duplicateMetas.mkString(" | ")} best: ${best}")
              for (hash <- duplicateHashes) {
                val meta = metasByHash.get(hash)
                if (!meta.isDefined || scores.getOrElse(hash, 0) < bestscore || 
                    (scores.getOrElse(hash, 0) == bestscore && best.year > 0 && best.year < meta.get.year)) {
                  if (meta.isDefined) {
                    debug(s"Overriding meta data entry ${meta.get} with ${best}")
                  } else {
                    debug(s"Overriding meta data for md5 ${hash} with ${best}")
                  }
                  val old = meta.getOrElse(best)
                  val authors =
                   if ((old.authors.isEmpty && best.authors.nonEmpty) || (old.authors.size < best.authors.size &&
                       (best.authors.map(normalize).exists(a => old.authors.map(normalize).exists(_.startsOrEndsWith(a)))))) best.authors
                   else old.authors
                  metasByHash(hash) = best.copy(authors = authors.sorted, hash = hash)
                } else if ((meta.get.authors.isEmpty && best.authors.nonEmpty) || (
                            meta.get.authors.size < best.authors.size &&
                            best.authors.map(normalize).exists(a => meta.get.authors.map(normalize).exists(_.startsOrEndsWith(a))) &&
                            ((best.publishers.isEmpty && meta.get.publishers.isEmpty) || best.publishers.map(normalize).exists(p => meta.get.publishers.map(normalize).exists(_.startsOrEndsWith(p)))) &&
                            ((best.year == 0 && meta.get.year == 0) || best.year == meta.get.year) &&
                            ((best.album.isEmpty && meta.get.album.isEmpty) ||
                            (normalizeAlbum(best) == normalizeAlbum(meta.get))))
                ) {
                  debug(s"Overriding authors for ${meta.get} with ${best.authors.sorted}")
                  metasByHash(hash) = meta.get.copy(authors = best.authors.sorted)
                }
              }
            }
          }
          remainingHashes = remainingHashes.filterNot(_ == cmp.hash)
          metas = metas.filterNot(_.hash == cmp.hash)
        }
      }

    for (iteration <- 1 to (if (processAgain) 3 else 1)) {
      // process connected components in parallel; audioTags within each component run sequentially
      components.par.foreach { component =>
        component.foreach { case (audioTag, entries, entriesByHash, hashes) =>
          processAudioTag(audioTag, entries, entriesByHash, hashes)
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
      meta.copy(_platform = platform)
    } else meta
  }).seq.toBuffer

  val allmetas = (finalMetas.par.flatMap(expandArticleVariants).seq ++ extraMetas)
    .filterNot(_.album.isEmpty)

  val metasWithAlbum = allmetas
    .filterNot(_.album.isEmpty)
    .groupBy(m => normalizeAlbum(m))

  val yearPublisher = allmetas
    .filterNot(_.year == 0)
    .filterNot(_.publishers.isEmpty)
    .par.flatMap(m => {
      m.publishers.map(normalize).flatMap { publisher =>
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
    .par.flatMap(m => {
      m.authors.map(normalize).flatMap { author =>
        Seq(
          (author, m)
        )
      }
    })
    .groupBy(_._1)
    .mapValues(_.map(_._2).toSet)

  finalMetas = finalMetas.par.map(m =>
    var meta = m
    boundary {
      if (meta.album.isEmpty) {
        break()
      }
      val key = normalizeAlbum(meta)
      val availableMetas = metasWithAlbum(key)
        .filterNot(_.hash == meta.hash)
        .filter(m => meta._platform.isEmpty || m._platform.isEmpty || m._platform.toLowerCase == meta._platform.toLowerCase)
      val availableTypes = availableMetas.map(_._type).filterNot(_.isEmpty).toSet
      var metas = availableMetas
        .filterNot(_.publishers.isEmpty)
        .filterNot(_.year == 0)
        .filter(m => meta.year == 0 || m.year == meta.year)
        .filter(m => (m._type.toLowerCase.startsWith("game") && meta._type.toLowerCase.startsWith("game")) || (!m._type.toLowerCase.startsWith("game") && !meta._type.toLowerCase.startsWith("game")) || (meta._type.isEmpty && availableTypes.size <= 1))
        .filter(m => m.authors.isEmpty || meta.authors.isEmpty || meta.authors.map(normalize).exists(a => m.authors.map(normalize).exists(_.startsOrEndsWith(a))))

      if (metas.isEmpty) {
        break()
      }

      if (metas.filter(_._platform.toLowerCase == meta._platform.toLowerCase).size >= 1) {
        metas = metas.filter(_._platform.toLowerCase == meta._platform.toLowerCase)
      }

      if (metas.filter(_.album.toLowerCase == meta.album.toLowerCase).size >= 1) {
        metas = metas.filter(_.album.toLowerCase == meta.album.toLowerCase)
      }

      val player = songlengths.songlengthsByMd5(meta.hash).head.player
      if (player == "uade" && metas.exists(_._platform == "Amiga") && metas.exists(m => m._platform.nonEmpty && m._platform != "Amiga")) {
        metas = metas.filter(m => m._platform.isEmpty || m._platform == "Amiga")
      }

      val cmp = metas.filter(_.album == meta.album).headOption.getOrElse(metas.head)
      val publishers = cmp.publishers.map(normalize).sorted.distinct
      val year = cmp.year

      lazy val yearPlatformTypeMatch = (metas.forall(m => m.year == year) || (
        metas.forall(m => m.year == 0 || Math.abs(m.year - year) <= 1) &&
        metas.forall(m => m._platform.isEmpty || m._platform == meta._platform) &&
        metas.forall(m => m._type.isEmpty || m._type == meta._type))
      )

      if (meta.publishers.isEmpty && meta.year == 0) {
        val authorMatches = meta.authors.map(normalize).flatMap(a => authorMetas.get(a)).flatten
          .filter(e =>
            meta._type.isEmpty || (e._type.toLowerCase.startsWith("game") && meta._type.toLowerCase.startsWith("game")) ||
            (!e._type.toLowerCase.startsWith("game") && !meta._type.toLowerCase.startsWith("game")))
          .filter(e => meta._platform.isEmpty || e._platform.toLowerCase == meta._platform.toLowerCase)
          .distinct

        if (!(meta.authors.isEmpty || authorMatches.nonEmpty)) {
          break()
        }
        if (metas.forall(m =>
            m.publishers.map(normalize).exists(p => publishers.exists(_.startsOrEndsWith(p)))) &&
            yearPlatformTypeMatch &&
            publishers.exists(p => yearPublisher.contains((year, p)))
        ) {
          debug(s"Filling publishers and year for ${meta.hash} - ${meta.album}: publishers ${meta.publishers.mkString(",")} -> ${cmp.publishers.mkString(",")}, year ${meta.year} -> ${year} source: ${cmp}")
          meta = meta.copy(publishers = cmp.publishers, year = year)
        }

      } else if (meta.publishers.isEmpty && meta.year != 0 && yearPlatformTypeMatch) { 
        if (metas.forall(m => m.publishers.map(normalize).exists(p => publishers.exists(_.startsOrEndsWith(p)))) &&
            publishers.exists(p => yearPublisher.contains((meta.year, p))) &&
            availableMetas.filter(m => meta.authors.isEmpty || m.authors.map(normalize).exists(a => meta.authors.map(normalize).exists(_.startsOrEndsWith(a)))).forall(m => m.year == 0 || m.year == meta.year)
        ) {
          debug(s"Filling publishers for ${meta.hash} - ${meta.album}: publishers ${meta.publishers.mkString(",")} -> ${cmp.publishers.mkString(",")} source: ${cmp}")
          meta = meta.copy(publishers = cmp.publishers)
        }

      } else if (meta.publishers.nonEmpty && meta.year == 0 && year != 0 && yearPlatformTypeMatch) {
        if (availableMetas.filter(m => meta.authors.isEmpty || m.authors.map(normalize).exists(a => meta.authors.map(normalize).exists(_.startsOrEndsWith(a)))).forall(m => m.year == 0 || m.year == year) &&
            meta.publishers.map(normalize).exists(p => publishers.exists(_.startsOrEndsWith(p))) &&
            publishers.exists(p => yearPublisher.contains((year, p)))
        ) {
          debug(s"Filling year for ${meta.hash} - ${meta.album}: year ${meta.year} -> ${year} source: ${cmp}")
          meta = meta.copy(year = year)
        }
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
      val availableMetas = metasWithAlbum(key)
        .filterNot(_.hash == meta.hash)
        .filter(m => meta._platform.isEmpty || m._platform.isEmpty || m._platform.toLowerCase == meta._platform.toLowerCase)
      val availableTypes = availableMetas.map(_._type).filterNot(_.isEmpty).toSet
      var metas = availableMetas
        .filter(m => (m._type.toLowerCase.startsWith("game") && meta._type.toLowerCase.startsWith("game")) || (!m._type.toLowerCase.startsWith("game") && !meta._type.toLowerCase.startsWith("game")) || (meta._type.isEmpty && availableTypes.size <= 1))
        .filter(m => m.publishers.map(normalize).exists(p => meta.publishers.map(normalize).exists(_.startsOrEndsWith(p))))
        .filter(m => m.year == meta.year)

      if (metas.isEmpty) {
        break()
      }

      if (metas.filter(_._platform.toLowerCase == meta._platform.toLowerCase).size >= 1) {
        metas = metas.filter(_._platform.toLowerCase == meta._platform.toLowerCase)
      }

      if (metas.filter(_.album.toLowerCase == meta.album.toLowerCase).size >= 1) {
        metas = metas.filter(_.album.toLowerCase == meta.album.toLowerCase)
      }
  
      if (metas.size > 1) {
        val audioHashes = metas.flatMap(m => audio.audioByMd5.get(m.hash).getOrElse(Buffer.empty).flatMap(_.audioHash)).toSet
        if (audioHashes.size > 1) {
          break()
        }
      }
      metas = metas
        .filterNot(_.authors.isEmpty)
        .filterNot(_.publishers.isEmpty)
        .filterNot(_.year == 0)

      if (metas.isEmpty) {
        break()
      }
      val cmp = metas.filter(_.album == meta.album).headOption.getOrElse(metas.head)
      val authors = cmp.authors.map(normalize).sorted.distinct
      if (!metas.forall(m => m.authors.map(normalize).sorted.distinct == authors)) {
        break()
      }
      debug(s"Filling authors for ${meta}: authors ${meta.authors.mkString(",")} -> ${cmp.authors.mkString(",")} source: ${cmp}")
      meta = meta.copy(authors = cmp.authors)
    }
    meta
  ).toBuffer.sortBy(_.hash).distinct
  finalMetas.seq.toBuffer
}
