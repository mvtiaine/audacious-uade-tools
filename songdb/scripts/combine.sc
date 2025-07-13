// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._

import convert._

def combineMetadata(
  amp: Buffer[MetaData],
  modland: Buffer[MetaData],
  unexotica: Buffer[MetaData],
  demozoo: Buffer[MetaData],
  oldexotica: Buffer[MetaData],
  wantedteam: Buffer[MetaData],
  modsanthology: Buffer[MetaData],
) = {
  val hashes = (
    amp.par.map(_.hash) ++
    modland.par.map(_.hash) ++
    unexotica.par.map(_.hash) ++
    demozoo.par.map(_.hash) ++
    oldexotica.par.map(_.hash) ++
    wantedteam.par.map(_.hash) ++
    modsanthology.par.map(_.hash)
  ).toSet

  val ampg = amp.groupBy(_.hash).par.mapValues(_.head)
  // canonize Falcon (PL) -> Falcon etc.
  val modlandg = modland.groupBy(_.hash).par.mapValues(v => v.head.copy(
    authors = v.head.authors.map(_.replaceAll(" \\(.*\\)$", "")))).seq
  val demozoog = demozoo.groupBy(_.hash).par.mapValues(_.head)
  val unexoticag = unexotica.groupBy(_.hash).par.mapValues(_.head)
  val oldexoticag = oldexotica.groupBy(_.hash).par.mapValues(_.head)
  val wantedteamg = wantedteam.groupBy(_.hash).par.mapValues(_.head)
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

  var metas = hashes.par.map { hash =>
    val a = ampg.get(hash)
    val m = modlandg.get(hash)
    val d = demozoog.get(hash)
    val u = unexoticag.get(hash)
    val o = oldexoticag.get(hash)
    val w = wantedteamg.get(hash)
    val ma = modsanthologyg.get(hash)
 
    def pickAuthor[T](
      a: Option[MetaData],
      b: Option[MetaData],
      c: Option[MetaData],
      d: Option[MetaData],
      e: Option[MetaData],
      g: Option[MetaData],
      h: Option[MetaData],
      f: MetaData => T) = {
      // pick only if has some non-empty authors
      def maybe(m: Option[MetaData]) =
        if (m.isDefined && m.get.authors.nonEmpty) m
        else None
      maybe(a).map(f)
      .orElse(maybe(b).map(f))
      .orElse(maybe(c).map(f))
      .orElse(maybe(d).map(f))
      .orElse(maybe(e).map(f))
      .orElse(maybe(g).map(f))
      .orElse(maybe(h).map(f))
    }

    def pick[T](
      a: Option[MetaData],
      b: Option[MetaData],
      c: Option[MetaData],
      d: Option[MetaData],
      e: Option[MetaData],
      g: Option[MetaData] = None,
      h: Option[MetaData] = None,
      f: MetaData => T) = {
      // pick only if has some non-author metadata
      def maybe(m: Option[MetaData]) =
        if (m.isDefined && (m.get.publishers.nonEmpty || m.get.album.nonEmpty || m.get.year != 0)) m
        else None

      maybe(a).map(f)
      .orElse(maybe(b).map(f))
      .orElse(maybe(c).map(f))
      .orElse(maybe(d).map(f))
      .orElse(maybe(e).map(f))
      .orElse(maybe(g).map(f))
      .orElse(maybe(h).map(f))
    }

    // authors: AMP > Demozoo > Modland > UnExotica > OldExotica > WantedTeam > ModsAnthology
    val authors = pickAuthor(a, d, m, u, o, w, ma, f = _.authors).getOrElse(Buffer.empty)
    // album/publishers/year: UnExotica > Demozoo > OldExotica > WantedTeam > AMP > Modland > ModsAnthology
    var album = pick(u, d, o, w, a, m, ma, f = _.album).getOrElse("")
    var publishers = pick(u, d, o, w, ma, f = _.publishers).getOrElse(Buffer.empty)

    def pickAlbum(m: Option[MetaData]) = {
      val pubs = publishers.map(_.replaceAll("[^A-Za-z0-9]","").toLowerCase)
      val pubsm = if (m.isDefined) m.get.publishers.map(_.replaceAll("[^A-Za-z0-9]","").toLowerCase) else Buffer.empty
      if (album.isEmpty && m.isDefined && (publishers.isEmpty ||
        pubs.exists(p => pubsm.contains(p)) ||
        pubsm.exists(p => pubs.contains(p))
      )) m.get.album
      else album
    }
  
    if (album.isEmpty) album = pickAlbum(u)
    if (album.isEmpty) album = pickAlbum(d)
    if (album.isEmpty) album = pickAlbum(o)
    if (album.isEmpty) album = pickAlbum(w)
    if (album.isEmpty) album = pickAlbum(a)
    if (album.isEmpty) album = pickAlbum(m)
    if (album.isEmpty) album = pickAlbum(ma)

    def pickPublishers(m: Option[MetaData]) =
      if (publishers.isEmpty && m.isDefined && (
        album.toLowerCase.replaceAll("[^a-z0-9]","").contains(m.get.album.toLowerCase.replaceAll("[^a-z0-9]","")) ||
        m.get.album.toLowerCase.replaceAll("[^a-z0-9]","").contains(album.toLowerCase.replaceAll("[^a-z0-9]",""))
      )) m.get.publishers
      else publishers

    if (publishers.isEmpty) publishers = pickPublishers(u)
    if (publishers.isEmpty) publishers = pickPublishers(d)
    if (publishers.isEmpty) publishers = pickPublishers(o)
    if (publishers.isEmpty) publishers = pickPublishers(w)
    if (publishers.isEmpty) publishers = pickPublishers(ma)

    var year = pick(u, d, o, w, ma, f = _.year).getOrElse(0)

    def pickYear(m: Option[MetaData]) = {
      val pubs = publishers.map(_.replaceAll("[^A-Za-z0-9]","").toLowerCase)
      val pubsm = if (m.isDefined) m.get.publishers.map(_.replaceAll("[^A-Za-z0-9]","").toLowerCase) else Buffer.empty
      if (year == 0 && m.isDefined && (
        album.toLowerCase.replaceAll("[^a-z0-9]","").contains(m.get.album.toLowerCase.replaceAll("[^a-z0-9]","")) ||
        m.get.album.toLowerCase.replaceAll("[^a-z0-9]","").contains(album.toLowerCase.replaceAll("[^a-z0-9]","")) ||
        pubs.exists(p => pubsm.contains(p)) ||
        pubsm.exists(p => pubs.contains(p))
      )) m.get.year
      else year
    }
  
    if (year == 0) year = pickYear(u)
    if (year == 0) year = pickYear(d)
    if (year == 0) year = pickYear(o)
    if (year == 0) year = pickYear(w)
    if (year == 0) year = pickYear(ma)

    MetaData(hash, authors, publishers, album, year)
  }

  // find metas which have common author(s) + album, add publishers and year if missing
  val metasByAuthorAlbumWithPublisherOrYear = metas
    .filterNot(_.authors.isEmpty)
    .filterNot(_.album.isEmpty)
    .filterNot(m => m.publishers.isEmpty && m.year == 0)
    .groupBy(m => (
      m.authors.map(_.toLowerCase.replaceAll("[^a-z0-9]","")),
      m.album
        .replace(" #"," ")
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
        .toLowerCase
    ))
    .flatMap { case (key, metas) =>
      key._1.map(a => (a, key._2) -> metas)
    }

  metas = metas.par.map(m =>
    if (m.authors.isEmpty || m.album.isEmpty || (!m.publishers.isEmpty && m.year != 0)) m
    else {
      val keys = m.authors.map(a => 
        (a.toLowerCase.replaceAll("[^a-z0-9]",""),
         m.album
           .replace(" #"," ")
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
           .toLowerCase)
      )

      val key = keys.find(metasByAuthorAlbumWithPublisherOrYear.contains(_))
      if (key.isDefined) {
        val metas = metasByAuthorAlbumWithPublisherOrYear(key.get)
        var publishers = if (m.publishers.isEmpty) metas.find(!_.publishers.isEmpty).map(_.publishers).getOrElse(Buffer.empty) else m.publishers
        if (!metas.forall(m => m.publishers.isEmpty
          || m.publishers.exists(p => publishers.contains(p))
          || publishers.exists(p => m.publishers.contains(p)))
        ) {
          System.err.println(s"WARN: publishers differ for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.publishers.mkString(",")} != ${metas.flatMap(_.publishers).mkString(",")}")
        }
        var year = if (m.year == 0) metas.filter(_.year != 0).map(_.year).toSeq.seq.sorted.headOption.getOrElse(0) else m.year
        if (!metas.forall(m => m.year == 0 || m.year == year)) {
          System.err.println(s"WARN: year differs for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.year} != ${metas.map(_.year).mkString(",")}")
        }
        m.copy(publishers = publishers, year = year)
      } else {
        m
      }
    }
  )
  
  // find metas which have common publisher(s) + album, add year if missing
  val metasByPublisherAlbumWithYear = metas
    .filterNot(_.publishers.isEmpty)
    .filterNot(_.album.isEmpty)
    .filter(_.year != 0)
    .groupBy(m => (
      m.publishers.map(_.toLowerCase.replaceAll("[^a-z0-9]","")),
      m.album
        .replace(" #"," ")
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
        .toLowerCase
    ))
    .flatMap { case (key, metas) =>
      key._1.map(p => (p, key._2) -> metas)
    }

  metas = metas.par.map(m =>
    if (m.album.isEmpty || m.publishers.isEmpty || m.year != 0) m
    else {
      val keys = m.publishers.map(p => 
        (p.toLowerCase.replaceAll("[^a-z0-9]",""),
        m.album
          .replace(" #"," ")
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
          .toLowerCase)
      )
      val key = keys.find(metasByPublisherAlbumWithYear.contains(_))
      if (key.isDefined) {
        val metas = metasByPublisherAlbumWithYear(key.get)
        var year = metas.filter(_.year != 0).map(_.year).toSeq.seq.sorted.headOption.getOrElse(0)
        if (!metas.forall(m => m.year == 0 || m.year == year)) {
          System.err.println(s"WARN: year differs for ${m.hash} - ${m.album} - ${m.publishers.mkString(",")} - ${m.year} != ${metas.map(_.year).mkString(",")}")
        }
        m.copy(year = year)
      } else {
        m
      }
    }
  )

  val metasByAlbumWithAuthorPublisherOrYear = metas
    .filterNot(_.album.isEmpty)
    .filterNot(m => m.publishers.isEmpty && m.year == 0)
    .groupBy(
      _.album
        .replace(" #"," ")
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
        .toLowerCase
    )

  metas = metas.par.map(m =>
    if (!m.authors.isEmpty || m.album.isEmpty || (!m.publishers.isEmpty && m.year != 0)) m
    else {
      val key = m.album
        .replace(" #"," ")
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
        .toLowerCase
      val metas = metasByAlbumWithAuthorPublisherOrYear.get(key)
      if (metas.isDefined && metas.get.size >= 1) {
        val authors = metas.get.head.authors.sorted
        if (metas.get.forall(_.authors.sorted.equals(authors))) {
          var publishers = if (m.publishers.isEmpty) metas.get.find(!_.publishers.isEmpty).map(_.publishers).getOrElse(Buffer.empty) else m.publishers
          if (!metas.get.forall(m => m.publishers.isEmpty
              || m.publishers.exists(p => publishers.contains(p))
              || publishers.exists(p => m.publishers.contains(p)))
          ) {
            System.err.println(s"WARN: publishers differ for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.publishers.mkString(",")} != ${metas.get.flatMap(_.publishers).mkString(",")}")
          }
          var year = if (m.year == 0) metas.get.filter(_.year != 0).map(_.year).toSeq.seq.sorted.headOption.getOrElse(0) else m.year
          if (!metas.get.forall(m => m.year == 0 || m.year == year)) {
            System.err.println(s"WARN: year differs for ${m.hash} - ${m.authors.mkString(",")} - ${m.album} - ${m.year} != ${metas.get.map(_.year).mkString(",")}")
          }
          m.copy(authors = authors, publishers = publishers, year = year)
        } else {
          m
        }
      } else {
        m
      }
    }
  )

  metas.toBuffer.sortBy(_.hash)
}
