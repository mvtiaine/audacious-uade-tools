#!/usr/bin/env -S scala-cli shebang

// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using file scripts/md5.sc
//> using file scripts/dedup.sc
//> using file scripts/convert.sc
//> using file scripts/pretty.sc
//> using file scripts/combine.sc

//> using file scripts/songlengths.sc
//> using file scripts/sources/sources.sc
//> using file scripts/sources/unexotica.sc
//> using file scripts/sources/amp.sc
//> using file scripts/sources/demozoo.sc
//> using file scripts/sources/modland.sc
//> using file scripts/sources/oldexotica.sc
//> using file scripts/sources/wantedteam.sc
//> using file scripts/sources/modsanthology.sc

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure

import md5._
import dedup._
import convert._
import pretty._
import combine._

implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

// 0 entry is special
lazy val idx2md5 = Buffer("0" * 12) ++ songlengths.db.sortBy(_.md5).map(_.md5.take(12)).distinct

var ampdata: Buffer[MetaData] = Buffer.empty
var modlanddata: Buffer[MetaData] = Buffer.empty
var unexoticadata: Buffer[MetaData] = Buffer.empty
var demozoodata: Buffer[MetaData] = Buffer.empty
var oldexoticadata: Buffer[MetaData] = Buffer.empty
var wantedteamdata: Buffer[MetaData] = Buffer.empty
var modsanthologydata: Buffer[MetaData] = Buffer.empty

def dedupMeta(entries: Buffer[MetaData], name: String) = {
  val dedupped = entries.groupBy(_.md5).map { case (md5, metas) =>
    if (metas.size > 1) {
      System.err.println(s"WARN: removing duplicate entries in ${name}, md5: ${metas.head.md5} entries: ${metas}")
    }
    val SORT = "\u0001"
    val meta = metas.sortBy(m => ("" +
     (if (m.year == 0) 9999 else m.year) + SORT +
     (if (m.authors.isEmpty) SEPARATOR else (10 - m.authors.size) + m.authors.mkString(SEPARATOR)) + SORT +
     (if (m.album.isEmpty) SEPARATOR else m.album) + SORT +
     (if (m.publishers.isEmpty) SEPARATOR else (10 - m.publishers.size) + m.publishers.mkString(SEPARATOR)) + SORT
    )).head
    MetaData(md5, meta.authors, meta.publishers, meta.album, meta.year)
  }.toBuffer
  dedupped
}

def processMetaTsvs(_entries: Buffer[MetaData], name: String) = {
  val entries = dedupMeta(_entries, name)
  // encoding does also deduplication
  val encoded = encodeMetaTsv(entries, name)
  val decoded = decodeMetaTsv(encoded, idx2md5)
  val pretty = createPrettyMetaTsv(decoded)
  Files.write(Paths.get(s"/tmp/songdb/${name}"), encoded.getBytes("UTF-8"))
  Files.write(Paths.get(s"/tmp/songdb/pretty/${name}"), pretty.getBytes("UTF-8"))
  assert(decoded == parsePrettyMetaTsv(pretty))
  assert(encoded == encodeMetaTsv(decoded, name))
  decoded
}

def _try[T](f: => T) = try {
  f
} catch {
  case e: Throwable =>
    e.printStackTrace()
    throw e
}

lazy val md5idxTsv = Future(_try {
  idx2md5.zipWithIndex.foreach { case (md5s, idx) =>
    val b64 = md5(md5s)
    val md5v = base64d(b64)
    val b24 = base64e24(idx, true)
    assert(_md5idx.get(md5s).isEmpty)
    _md5idx(md5s) = b24
    assert(_idxmd5.get(b24).isEmpty)
    _idxmd5(b24) = md5s
  }

  val encoded = encodeMd5IdxTsv(idx2md5)
  Files.write(Paths.get("/tmp/songdb/md5idx.tsv"), encoded.getBytes("UTF-8"))
})

lazy val songlengthsTsvs = Future(_try {
  val entries = songlengths.db.sortBy(_.md5).par.map(e =>
    SongInfo(
      e.md5.take(12),
      e.minsubsong,
      e.subsongs.sortBy(_.subsong).map(s =>
        SubsongInfo(
          s.songlength,
          s.songend,
        )
      ).toBuffer
    )
  ).toBuffer.distinct

  // encoding does also deduplication
  val encoded = encodeSonglengthsTsv(entries)
  val decoded = decodeSonglengthsTsv(encoded, idx2md5)
  val pretty = createPrettySonglengthsTsv(decoded)
  Files.write(Paths.get("/tmp/songdb/songlengths.tsv"), encoded.getBytes("UTF-8"))
  Files.write(Paths.get("/tmp/songdb/pretty/songlengths.tsv"), pretty.getBytes("UTF-8"))
  assert(decoded == parsePrettySonglengthsTsv(pretty))
  assert(encoded == encodeSonglengthsTsv(decoded))
})

lazy val modinfosTsvs = Future(_try {
  val entries = songlengths.db.sortBy(_.md5).par.map { e =>
    ModInfo(
      e.md5.take(12),
      e.format,
      e.channels
    )
  }.toBuffer.distinct

  // encoding does also deduplication
  val encoded = encodeModInfosTsv(entries)
  val decoded = decodeModInfosTsv(encoded, idx2md5)
  val pretty = createPrettyModInfosTsv(decoded)
  Files.write(Paths.get("/tmp/songdb/modinfos.tsv"), encoded.getBytes("UTF-8"))
  Files.write(Paths.get("/tmp/songdb/pretty/modinfos.tsv"), pretty.getBytes("UTF-8"))
  assert(decoded == parsePrettyModInfosTsv(pretty))
  assert(encoded == encodeModInfosTsv(decoded))
})

lazy val ampTsvs = Future(_try {
  val entries = amp.metas.groupBy(m => (m.md5, m.path)).par.map { case ((md5, path), m) =>
    var best = m.head
    if (m.size > 1) {
      best = m.maxBy(_.extra_authors.size)
    }
    best
  }.par.flatMap(m =>
    val path = m.path.substring(m.path.indexOf("/") + 1, m.path.lastIndexOf("/"))
    if (!m.extra_authors.isEmpty && !m.extra_authors.forall(a => a.isEmpty || a == "Unknown Composers")) {
      Some(MetaData(
        m.md5.take(12),
        m.extra_authors.sorted.filterNot(_.isEmpty).toBuffer,
        Buffer.empty,
        m.album,
        0
      ))
    } else None
  ).toBuffer.distinct

  ampdata = processMetaTsvs(entries, "amp.tsv")
})

lazy val modlandTsvs = Future(_try {
  val entries = sources.modland.sortBy(_.md5).par.flatMap { e =>
    var path =
      if (e.path.startsWith("Ad Lib/")) e.path.substring("Ad Lib/".length)
      else e.path
    val format = path.substring(0, path.indexOf("/"))
    path = path.substring(path.indexOf("/") + 1, path.lastIndexOf("/"))
    if (path != "- unknown" && path != "_unknown") {
      modland.parseModlandAuthorAlbum(path).map { case (authors, album) =>
        MetaData(
          e.md5.take(12),
          authors.sorted.toBuffer,
          Buffer.empty,
          album,
          0
        )
      }
    } else None
  }.toBuffer.distinct

  modlanddata = processMetaTsvs(entries, "modland.tsv")
})

lazy val unexoticaTsvs = Future(_try {
  val entries = unexotica.metas.par.map { m =>
    val md5 = m._1
    val path = m._2
    val authorAlbum = path.substring(path.indexOf("/") + 1, path.lastIndexOf("/")).split("/")
    val authors = Buffer(unexotica.transformAuthors(authorAlbum(0)))
    val filesize = m._3
    val meta = m._4
    val album = unexotica.transformAlbum(meta, authorAlbum)
    val publishers = unexotica.transformPublishers(meta)
    val year = meta.year.fold(_.toString, _.toString)
    MetaData(
      md5.take(12),
      authors,
      publishers,
      album,
      if (year != "Unknown") year.toInt else 0
    )
  }.toBuffer.distinct

  unexoticadata = processMetaTsvs(entries, "unexotica.tsv")
})

lazy val demozooTsvs = Future(_try {
  val entries = demozoo.metas.par.flatMap { case (md5, m) =>
    val dates = Seq(m.modDate, m.prodDate, m.partyDate.getOrElse("")).filterNot(_.isEmpty)
    val earliestDate = if (dates.isEmpty) "" else dates.min
    val authors = m.authors.filterNot(_ == "?").sorted.toBuffer
    val useProd = !m.prod.isEmpty && (m.prodDate == earliestDate || m.partyDate.getOrElse("") != earliestDate)
    val info = MetaData(
      md5.take(12),
      if (authors.forall(_.trim.isEmpty)) Buffer.empty else authors,
      ((m.prodPublishers, m.party, m.modPublishers) match {
        case (prod,_,_) if useProd =>
          if (prod.forall(_.trim.isEmpty)) Buffer.empty else prod.toBuffer
        case (_,party,_) if !party.isEmpty =>
          Buffer(party.get)
        case (_,_,mod) if !mod.isEmpty =>
          if (mod.forall(_.trim.isEmpty)) Buffer.empty else mod.toBuffer
        case _ => Buffer.empty
      }).sorted,
      if (useProd) m.prod.trim else "",
      if (!earliestDate.isEmpty) earliestDate.substring(0,4).toInt else 0
      //if (!m.prodPlatforms.isEmpty) m.prodPlatforms else m.modPlatform
    )
    info match {
      case MetaData(_, Buffer(), Buffer(), "", 0) => None
      case _ => Some(info)
    }
  }.toBuffer.distinct

  demozoodata = processMetaTsvs(entries, "demozoo.tsv")
})

lazy val oldexoticaTsvs = Future(_try {
  val entries = oldexotica.metas.par.flatMap { m =>
    val authors = oldexotica.transformAuthors(m)
    val publishers = oldexotica.transformPublishers(m)
    val album = oldexotica.transformAlbum(m)
    val year = m.year.getOrElse(0)
    if (authors.isEmpty && publishers.isEmpty && album.isEmpty && year == 0) None
    else Some(MetaData(
      m.md5.take(12),
      authors,
      publishers,
      album,
      year
    ))
  }.toBuffer.distinct

  oldexoticadata = processMetaTsvs(entries, "oldexotica.tsv")
})

lazy val wantedteamTsvs = Future(_try {
  val entries = wantedteam.metas.par.flatMap { m =>
    if (m.authors.isEmpty && m.publishers.isEmpty && m.album.isEmpty && !m.year.isDefined) None
    else Some(MetaData(
      m.md5.take(12),
      m.authors,
      m.publishers,
      m.album,
      m.year.getOrElse(0)
    ))
  }.toBuffer.distinct

  wantedteamdata = processMetaTsvs(entries, "wantedteam.tsv")
})

lazy val modsanthologyTsvs = Future(_try {
  val entries = modsanthology.metas.par.flatMap { m =>
    if (m.authors.isEmpty && m.publishers.isEmpty && m.album.isEmpty && !m.year.isDefined) None
    else Some(MetaData(
      m.md5.take(12),
      m.authors,
      m.publishers,
      m.album,
      m.year.getOrElse(0)
    ))
  }.toBuffer.distinct

  modsanthologydata = processMetaTsvs(entries, "modsanthology.tsv")
})

// needs to be processed first
Await.ready(md5idxTsv, Duration.Inf)
val future = Future.sequence(
  Seq(md5idxTsv,
      songlengthsTsvs,
      modinfosTsvs,
      ampTsvs,
      modlandTsvs,
      unexoticaTsvs,
      demozooTsvs,
      oldexoticaTsvs,
      wantedteamTsvs,
      modsanthologyTsvs
  )

) andThen {
  case _ =>
    val combined = combineMetadata(
      ampdata,
      modlanddata,
      unexoticadata,
      demozoodata,
      oldexoticadata,
      wantedteamdata,
      modsanthologydata
    )
    processMetaTsvs(combined, "combined.tsv")
}

future onComplete {
  case Failure(e) =>
    e.printStackTrace()
    System.exit(1)
  case Success(value) =>
    System.out.println("Songdb files created to /tmp/songdb/")
}

Await.ready(future, Duration.Inf)
