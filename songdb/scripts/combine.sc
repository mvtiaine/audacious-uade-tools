// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import java.util.concurrent.ConcurrentHashMap
import java.util.regex.Pattern
import scala.collection.Map
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSet
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala
import spire.math._

import convert._
import audio._
import chromaprint._
import tosec._
import whdload._

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

def withArticleVariants(s: String): Buffer[String] = {
  val withoutArticle = removeArticle(s)
  if (withoutArticle != s) Buffer(s, withoutArticle)
  else Buffer(s)
}

def withArticleVariants(items: Buffer[String]): Buffer[String] = {
  items.flatMap(withArticleVariants).distinct
}

def expandArticleVariants(m: MetaData): Set[MetaData] = {
  // Generate all combinations of article variants
  // For authors/publishers: if any item has an article, create versions with original and without
  val authorsList = if (m.authors.isEmpty) {
    Seq(m.authors)
  } else {
    val withVariants = withArticleVariants(m.authors)
    if (withVariants == m.authors) Seq(m.authors)
    else Seq(m.authors, withVariants)
  }
  
  val albumList = withArticleVariants(m.album)
  
  val publishersList = if (m.publishers.isEmpty) {
    Seq(m.publishers)
  } else {
    val withVariants = withArticleVariants(m.publishers)
    if (withVariants == m.publishers) Seq(m.publishers)
    else Seq(m.publishers, withVariants)
  }
  
  (for {
    authors <- authorsList
    album <- albumList
    publishers <- publishersList
  } yield m.copy(authors = authors, album = album, publishers = publishers)).toSet
}

def normalize(s: String) = s
  .replaceAll("[^A-Za-z0-9]","")
  .replaceAll("ü","ue")
  .replaceAll("Ø","o")
  .replaceAll("ë","e")
  .toLowerCase
def normalizeAlbum(m: MetaData): String = normalizeAlbum(m.album, m.publishers)
def normalizeAlbum(album: String, publishers: Buffer[String]): String = {
  var a = album
  publishers.foreach(p =>
    a = a.replaceAll(s"^${Pattern.quote(p)} ", "")
  )
  a
  .replaceAll(" PC$", "")
  .replaceAll(" v[0-9]+(\\.[0-9]+)*$","")
  .replaceAll("\\(.*\\)$","")
  .replaceAll(" #(.*)$"," $1")
  .replaceAll(" 0([1-9][0-9])$"," $1")
  .replaceAll(" 00([0-9])$"," $1")
  .replaceAll(" 0([0-9])$"," $1")
  .replaceAll(" 0$","")
  .replaceAll(" 1$","")
  .replaceAll(" [Ii]$","")
  .replaceAll(" [Ii][Ii]$"," 2")
  .replaceAll(" [Ii][Ii][Ii]$"," 3")
  .replaceAll(" [Ii][Vv]$"," 4")
  .replaceAll(" [Vv]$"," 5")
  .replaceAll(" [Vv][Ii]$"," 6")
  .replaceAll(" [Vv][Ii][Ii]$"," 7")
  .replaceAll(" [Vv][Ii][Ii][Ii]$"," 8")
  .replaceAll(" [Ii][Xx]$"," 9")
  .replaceAll("[^A-Za-z0-9]","")
  .trim
  .toLowerCase
  .replaceAll("ü","ue")
  .replaceAll("Ø","o")
  .replaceAll("ë","e")
  .trim
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
) = {
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

  lazy val fujiology2 = removeCompilations(fujiology)

  val allmetas = (
    amp ++
    modland ++
    unexotica ++
    demozoo ++
    oldexotica ++
    wantedteam ++
    modsanthology ++
    fujiology2 ++
    tosecmusic
  ).groupBy(_.hash).par.mapValues(_.distinct).seq
  val allmetas2 = allmetas.flatMap(_._2.toSet).toSet

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
  //val tosecmusicg = tosecmusic.groupBy(_.hash).par.mapValues(_.head).seq

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

  for (pass <- 1 to allMetaSources.size) {

    def trace(msg: String): Unit = {
      //System.err.println(s"TRACE ($pass): $msg")
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

      debug(s"initial pick for $hash -> authors: ${authors}, album: '${album}', publishers: ${publishers}, year: ${year}")
      MetaData(hash, authors, publishers, album, year)
    }
  
    // Duplicate entries with "The X", "An X", "A X" to include both versions
    val allmetas3 = (allmetas2 ++ metas ++ tosecMetas ++ whdloadMetas)
      .flatMap(expandArticleVariants)

    val yearAlbumPublishers = allmetas3
      .filterNot(_.album.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.year, normalizeAlbum(m), m.publishers.map(normalize).sorted.distinct))
      .toSet
  
    val yearAlbum = allmetas3
      .filterNot(_.album.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.year, normalizeAlbum(m)))
      .toSet

    val albumPublishers = allmetas3
      .filterNot(_.album.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .map(m => (normalizeAlbum(m), m.publishers.map(normalize).sorted.distinct))
      .toSet

    val authorsAlbumPublishers = allmetas3
      .filterNot(_.authors.isEmpty)
      .filterNot(_.album.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .map(m => (m.authors.map(normalize).sorted.distinct, normalizeAlbum(m), m.publishers.map(normalize).sorted.distinct))
      .toSet
  
    val authorsAlbumYear = allmetas3
      .filterNot(_.authors.isEmpty)
      .filterNot(_.album.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.authors.map(normalize).sorted.distinct, normalizeAlbum(m), m.year))
      .toSet

    val authorsYearPublishers = allmetas3
      .filterNot(_.authors.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.authors.map(normalize).sorted.distinct, m.year, m.publishers.map(normalize).sorted.distinct))
      .toSet

    val authorsYearNoAlbum = allmetas3
      .filter(_.album.isEmpty)
      .filterNot(_.authors.isEmpty)
      .filterNot(_.year == 0)
      .map(m => (m.authors.map(normalize).sorted.distinct, m.year))
      .toSet

    val authorsPublishersNoAlbum = allmetas3
      .filter(_.album.isEmpty)
      .filterNot(_.authors.isEmpty)
      .filterNot(_.publishers.isEmpty)
      .map(m => (m.authors.map(normalize).sorted.distinct, m.publishers.map(normalize).sorted.distinct))
      .toSet

    metas = metas.par.map { meta =>
      val hash = meta.hash
      val authors = meta.authors
      var album = meta.album
      var publishers = meta.publishers
      var year = meta.year

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
              e._2 == normalizeAlbum(album, m.get.publishers) &&
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
              e._1 == normalizeAlbum(album, m.get.publishers) &&
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
              e._2 == normalizeAlbum(album, publishers) &&
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
              e._2 == (normalizeAlbum(album, Buffer.empty)))
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

      MetaData(meta.hash, meta.authors, publishers, album, year)
    }
  
    for (iteration <- 1 to 2) {
      // find metas which have common author(s) + album, add publishers and year if missing
      val metasByAuthorAlbumWithPublisherOrYear = (allmetas2 ++ metas ++ tosecMetas ++ whdloadMetas)
        .flatMap(expandArticleVariants)
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
          trace(s"(1) ${m.hash} keys: ${keys}")
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
            trace(s"(1) ${m.hash} metas: ${metas.seq} key: ${key}")
            m.copy(publishers = publishers, year = year)
          } else {
            m
          }
        }
      )
  
      // find metas which have common publisher(s) + album, add year if missing
      val metasByPublisherAlbumWithYear = (allmetas2 ++ metas ++ tosecMetas ++ whdloadMetas)
        .flatMap(expandArticleVariants)
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
          trace(s"(2) ${m.hash} keys: ${keys}")
          val key = keys.find(metasByPublisherAlbumWithYear.contains(_))
          if (key.isDefined) {
            val metas = metasByPublisherAlbumWithYear(key.get)
            var year = if (m.year == 0) pickMostCommonYear(metas) else m.year
            if (!metas.forall(m => m.year == 0 || m.year == year)) {
              warn(s"(2) year differs for ${m.hash} - ${m.album} - ${m.publishers.mkString(",")} - ${m.year} != ${metas.map(_.year).mkString(",")}")
              m
            } else {
              debug(s"(2) overriding year for ${m.hash} - ${m.album} - ${m.publishers. mkString(",")}: year ${m.year} -> ${year}")
              trace(s"(2) ${m.hash} metas: ${metas.seq} key: ${key}")
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
      val metasByAlbumWithAuthorPublisherOrYear = (allmetas2 ++ metas ++ tosecMetas ++ whdloadMetas)
        .flatMap(expandArticleVariants)
        .filterNot(_.album.isEmpty)
        .filterNot(m => m.publishers.isEmpty && m.year == 0)
        .groupBy(m => normalizeAlbum(m))

      metas = metas.par.map(m => {
        if (m.authors.nonEmpty || m.album.isEmpty || (m.publishers.nonEmpty && m.year != 0) ||
           (m.album.nonEmpty && m.authors.isEmpty && m.publishers.isEmpty && m.year == 0)) m
        else {
          val key = normalizeAlbum(m)
          trace(s"(3) ${m.hash} key: ${key}")
          val metas = metasByAlbumWithAuthorPublisherOrYear.get(key)
          if (metas.isDefined && metas.get.size >= 1) {
            val authors = {
              val grouped = metas.get.groupBy(_.authors.sorted)
              grouped.seq.view.mapValues(_.size).maxBy(_._2)._1
            }
            // for any metas which have a different authors, check if there exists an entry in some source with those authors
            if (metas.get.forall(_.authors.map(normalize).sorted == authors.map(normalize)) ||
                metas.get.filterNot(_.hash.isEmpty).filterNot(_.authors.map(normalize).sorted == authors.map(normalize)).forall(da => allmetas(da.hash).exists(_.authors.map(normalize).sorted == authors.map(normalize)))) {
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
              trace(s"(3) ${m.hash} metas: ${metas.get.seq} key: ${key}")
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
    audio.audioByAudioTags.par.map { case (audioTag, entries) =>
      trace(s"Processing audio tag ${audioTag} with entries: ${entries.map(_.copy(audioChromaprint = "", audioHash = ""))}")
      lazy val entriesByHash = entries.groupBy(_.md5)
      var hashes = entries.map(_.md5).distinct
      var metas = hashes.flatMap(h => metasByHash.get(h))
      while (metas.nonEmpty && hashes.nonEmpty) {
        var cmp = metas.head
        if (!metas.forall(_ == metas.head) || hashes.size != metas.size) {
          val cmpAudioEntries = entriesByHash(cmp.hash).distinctBy(_.normalizedSubsong)
          var duplicateHashes = mutable.Buffer.empty[String]
          for (hash <- hashes) {
            var audioEntries = entriesByHash(hash).distinctBy(_.normalizedSubsong)
            assert(audioEntries.size == cmpAudioEntries.size)
            var duplicate = true
            for (i <- 0 until audioEntries.size) {
              if (cmpAudioEntries(i).audioChromaprint != audioEntries(i).audioChromaprint) {
                val threshold = if (audioEntries(i).audioBytes > 2 * 11025 * 12) 0.9 else 0.99
                val similarity = chromaSimilarity(cmpAudioEntries(i).audioChromaprint, audioEntries(i).audioChromaprint, threshold)
                if (similarity < threshold) {
                  duplicate = false
                  trace(s"Chromaprint similarity for ${hash} vs ${cmp.hash} subsong ${i} is too low: ${similarity}")
                } else {
                  trace(s"Chromaprint similarity for ${hash} vs ${cmp.hash} subsong ${i}: ${similarity}")
                }
              } else if (cmpAudioEntries(i).audioHash != audioEntries(i).audioHash) {
                duplicate = false
                trace(s"Audio hash mismatch for ${hash} subsong ${i} vs ${cmp.hash} subsong ${i}: ${cmpAudioEntries(i).audioHash} vs ${audioEntries(i).audioHash}")
              }
            }
            if (duplicate) {
              duplicateHashes += hash
            }
          }
          duplicateHashes = duplicateHashes.sorted.distinct
          val duplicateMetas = duplicateHashes.flatMap(h => metasByHash.get(h)).distinct
          val anyMetadata = duplicateMetas.exists(m => m.authors.nonEmpty || m.publishers.nonEmpty || m.album.nonEmpty || m.year != 0)
          if (anyMetadata && duplicateHashes.size > 1) {
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
              authorSources
                .flatMap(source => candidates.find(c => source.get(c.hash).exists(src =>
                  (src.authors == c.authors || src.authors.map(normalize).exists(a => c.authors.map(normalize).exists(_.startsOrEndsWith(a)))) &&
                  (src.publishers == c.publishers || src.publishers.map(normalize).exists(p => c.publishers.map(normalize).exists(_.startsOrEndsWith(p)))) &&
                  normalizeAlbum(src) == normalizeAlbum(c) &&
                  src.year == c.year
              ))).headOption
            }

            var best = if (byyear.nonEmpty) {
              val grouped = byyear.groupBy(m => (m.publishers.map(normalize).sorted, normalizeAlbum(m)))
              val majority = grouped.view.mapValues(_.size).maxBy(_._2)._1
              val candidates = grouped(majority)
              if (candidates.size > 1) {
                pickBySourcePriority(candidates).getOrElse(candidates.maxBy(_.authors.size))
              } else {
                candidates.maxBy(_.authors.size)
              }
            } else {
              // fallback: use majority voting on publisher/album for all bestmetas
              val grouped = bestmetas.groupBy(m => (m.publishers.map(normalize).sorted, normalizeAlbum(m)))
              if (grouped.size > 1) {
                val majority = grouped.view.mapValues(_.size).maxBy(_._2)._1
                val candidates = grouped(majority)
                if (candidates.size > 1) {
                  pickBySourcePriority(candidates).getOrElse(candidates.maxBy(_.authors.size))
                } else {
                  candidates.maxBy(_.authors.size)
                }
              } else {
                // fallback: use majority voting on authors for all bestmetas
                val grouped = bestmetas.groupBy(m => m.authors.map(normalize).sorted)
                if (grouped.size > 1) {
                  val majority = grouped.view.mapValues(_.size).maxBy(_._2)._1
                  val candidates = grouped(majority)
                  if (candidates.size > 1) {
                    pickBySourcePriority(candidates).getOrElse(candidates.head)
                  } else {
                    candidates.head
                  }
                } else {
                  bestmetas.maxBy(_.authors.size)
                }
              }
            }
            // use the most common authors as best
            lazy val authorGroups = duplicateMetas.groupBy(_.authors.sorted).filter(_._1.nonEmpty)
            lazy val bestAuthors = if (authorGroups.nonEmpty) {
              val maxCount = authorGroups.view.mapValues(_.size).maxBy(_._2)._2
              val tiedAuthors = authorGroups.filter(_._2.size == maxCount).keys.toSeq
              if (tiedAuthors.size == 1) {
                tiedAuthors.head
              } else {
                // Pick first authors found in authorSources priority order
                authorSources.view.flatMap { source =>
                  tiedAuthors.filter { authors =>
                    duplicateHashes.exists(hash =>
                      source.get(hash).exists(_.authors.sorted == authors)
                    )
                  }.headOption
                }.headOption.getOrElse(tiedAuthors.head)
              }
            } else {
              Buffer.empty
            }
            if (best.authors.isEmpty)
              best = best.copy(authors = bestAuthors)
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
        hashes = hashes.filterNot(_ == cmp.hash)
        metas = metas.filterNot(_.hash == cmp.hash)
      }
    }
  }
  metasByHash.values.toBuffer.sortBy(_.hash).distinct
}
