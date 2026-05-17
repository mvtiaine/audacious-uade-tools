#!/usr/bin/env -S scala-cli shebang --suppress-warning-directives-in-multiple-files -q -J --sun-misc-unsafe-memory-access=allow -J -Xmx64G -J -XX:+UseStringDeduplication -J -XX:+UseCompactObjectHeaders -XX:TrustFinalNonStaticFields

// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using file scripts/md5.sc
//> using file scripts/dedup.sc
//> using file scripts/convert.sc
//> using file scripts/pretty.sc
//> using file scripts/combine.sc
//> using file scripts/xxh32.sc
//> using file scripts/chromaprint.sc
//> using file scripts/normalization.sc

//> using file scripts/songlengths.sc
//> using file scripts/sources/sources.sc
//> using file scripts/sources/unexotica.sc
//> using file scripts/sources/amp.sc
//> using file scripts/sources/demozoo.sc
//> using file scripts/sources/modland.sc
//> using file scripts/sources/oldexotica.sc
//> using file scripts/sources/wantedteam.sc
//> using file scripts/sources/modsanthology.sc
//> using file scripts/sources/tosecmusic.sc
//> using file scripts/sources/fujiology.sc
//> using file scripts/sources/tosec.sc
//> using file scripts/sources/whdload.sc
//> using file scripts/sources/wikipedia.sc
//> using file scripts/sources/exodos.sc
//> using file scripts/sources/audio.sc

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.util.Success
import scala.util.Failure

import md5._
import dedup._
import convert._
import pretty._
import combine._
import xxh32._
import audio._
import chromaprint._
import tosecmusic._
import fujiology._
import demozoo._

implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

val DEST = "/tmp/songdb/"

// shutup warning
System.setProperty("log4j.provider", "org.apache.logging.log4j.simple.internal.SimpleProvider")

// init audio fingerprints eagerly to reduce memory usage
audio.duplicateSubsongsByPlayerAndMd5

// 0 entry is special
lazy val idx2md5 = Buffer("0" * 12) ++ songlengths.db.sortBy(_.md5).map(_.md5.take(12)).distinct
lazy val idx2xxh32 = Buffer("0" * 12) ++ songlengths.db.map(e => md5ToXxh32(e.md5.take(12))).sorted.distinct

var ampdata: Buffer[MetaData] = Buffer.empty
var modlanddata: Buffer[MetaData] = Buffer.empty
var unexoticadata: Buffer[MetaData] = Buffer.empty
var demozoodata: Buffer[MetaData] = Buffer.empty
var oldexoticadata: Buffer[MetaData] = Buffer.empty
var wantedteamdata: Buffer[MetaData] = Buffer.empty
var modsanthologydata: Buffer[MetaData] = Buffer.empty
var tosecmusicdata: Buffer[MetaData] = Buffer.empty
var fujiologydata: Buffer[MetaData] = Buffer.empty

val globalLeftovers = new java.util.concurrent.ConcurrentLinkedQueue[MetaData]()

def dedupMeta(entries: Buffer[MetaData], name: String): Buffer[MetaData] = {
  val allMetas = entries.groupBy(_.hash).flatMap { case (hash, metas) =>
    if (metas.size > 1) {
      System.err.println(s"WARN: removing duplicate entries in ${name}, hash: ${metas.head.hash} entries: ${metas}")
    }
    
    val scoredMetas = metas.map(e => 
      (e, (if (e.authors.nonEmpty) 1 else 0) + (if (e.publishers.nonEmpty) 1 else 0) + (if (e.album.nonEmpty) 1 else 0) + (if (e.year > 0) 1 else 0))
    )
    val bestscore = scoredMetas.map(_._2).max
    val bestMetasForScore = scoredMetas.filter(_._2 == bestscore).map(_._1)

    val SORT = "\u0001"
    val bestMeta = bestMetasForScore.sortBy(m => ("" +
     (if (m.year == 0) 9999 else m.year) + SORT +
     (if (m.authors.isEmpty) SEPARATOR else (10 - m.authors.size) + m.authors.mkString(SEPARATOR)) + SORT +
     (if (m.album.isEmpty) SEPARATOR else m.album) + SORT +
     (if (m.publishers.isEmpty) SEPARATOR else (10 - m.publishers.size) + m.publishers.mkString(SEPARATOR)) + SORT
    )).head

    val leftovers = metas.filter(_ != bestMeta)
    leftovers.foreach(globalLeftovers.add)

    val bestOpt = if (bestMeta.authors.isEmpty && bestMeta.publishers.isEmpty && bestMeta.album.isEmpty && bestMeta.year == 0) {
      None
    } else {
      Some(MetaData(hash, bestMeta.authors, bestMeta.publishers, bestMeta.album, bestMeta.year, bestMeta._type, bestMeta._platform))
    }
    
    bestOpt.toSeq
  }.toBuffer
  allMetas
}

def processMetaTsvs(_entries: Buffer[MetaData], name: String, allTsvs: Boolean = false): Buffer[MetaData] = {
  val dedupped = dedupMeta(_entries, name)
  // encoding does also deduplication
  val encoded = encodeMetaTsv(dedupped, name, _md5idx)
  val decoded = decodeMetaTsv(encoded, idx2md5)
  val pretty = createPrettyMetaTsv(decoded)

  Files.write(Paths.get(s"$DEST/pretty/md5/${name}"), pretty.getBytes("UTF-8"))

  assert(decoded == parsePrettyMetaTsv(pretty))
  assert(encoded == encodeMetaTsv(decoded, name, _md5idx))

  if (allTsvs) {
    val xxh32 = metasToXxh32(decoded)
    val xxh32Encoded = encodeMetaTsv(xxh32, name + ".xxh32", _xxh32idx)
    val xxh32Decoded = decodeMetaTsv(xxh32Encoded, idx2xxh32)
    val xxh32Pretty = createPrettyMetaTsv(xxh32)

    Files.write(Paths.get(s"$DEST/pretty/xxh32/${name}"), xxh32Pretty.getBytes("UTF-8"))
    Files.write(Paths.get(s"$DEST/encoded/xxh32/${name}"), xxh32Encoded.getBytes("UTF-8"))

    assert(xxh32Decoded == parsePrettyMetaTsv(xxh32Pretty))
    assert(xxh32Encoded == encodeMetaTsv(xxh32, name + ".xxh32", _xxh32idx))
  }

  dedupped
}

def _try[T](f: => T) = try {
  f
} catch {
  case e: Throwable =>
    e.printStackTrace()
    throw e
}

lazy val md5idx = Future(_try {
  idx2md5.zipWithIndex.foreach { case (md5s, idx) =>
    val b64 = md5(md5s)
    val md5v = base64d(b64)
    val b24 = base64e24(idx, true)
    assert(_md5idx.get(md5s).isEmpty)
    _md5idx(md5s) = b24
    assert(_idxmd5.get(b24).isEmpty)
    _idxmd5(b24) = md5s
  }
})

lazy val xxh32idxTsv = Future(_try {
  idx2xxh32.zipWithIndex.foreach { case (xxh32s, idx) =>
    val b64 = xxh32(xxh32s)
    val xxh32v = base64d(b64)
    val b24 = base64e24(idx, true)
    assert(_xxh32idx.get(xxh32s).isEmpty)
    _xxh32idx(xxh32s) = b24
    assert(_idxxxh32.get(b24).isEmpty)
    _idxxxh32(b24) = xxh32s
  }

  val encoded = encodeHashIdxTsv(idx2xxh32)
  Files.write(Paths.get(s"$DEST/encoded/xxh32/xxh32idx.tsv"), encoded.getBytes("UTF-8"))
})

lazy val songlengthsTsvs = Future(_try {
  val entries = songlengths.db.sortBy(_.md5).par.map(e => {
    val md5 = e.md5.take(12)
    val duplicates = audio.duplicateSubsongsByPlayerAndMd5.getOrElse((e.player, md5), scala.collection.mutable.SortedSet[Int]())
    SongInfo(
      md5,
      e.minsubsong,
      e.subsongs.sortBy(_.subsong).map(s =>
        SubsongInfo(
          s.songlength,
          s.songend,
          e.subsongs.size > duplicates.size && duplicates.contains(s.subsong),
        )
      ).toBuffer
    )
  }).toBuffer.distinct

  // encoding does also deduplication
  val encoded = encodeSonglengthsTsv(entries, _md5check)
  val decoded = decodeSonglengthsTsv(encoded, idx2md5)
  val pretty = createPrettySonglengthsTsv(decoded)

  val xxh32 = songlengthsToXxh32(decoded)
  val xxh32Encoded = encodeSonglengthsTsv(xxh32, _xxh32check)
  val xxh32Decoded = decodeSonglengthsTsv(xxh32Encoded, idx2xxh32)
  val xxh32Pretty = createPrettySonglengthsTsv(xxh32)

  Files.write(Paths.get(s"$DEST/encoded/xxh32/songlengths.tsv"), xxh32Encoded.getBytes("UTF-8"))
  Files.write(Paths.get(s"$DEST/pretty/md5/songlengths.tsv"), pretty.getBytes("UTF-8"))
  Files.write(Paths.get(s"$DEST/pretty/xxh32/songlengths.tsv"), xxh32Pretty.getBytes("UTF-8"))

  assert(decoded == parsePrettySonglengthsTsv(pretty))
  assert(encoded == encodeSonglengthsTsv(decoded, _md5check))
  assert(xxh32Decoded == parsePrettySonglengthsTsv(xxh32Pretty))
  assert(xxh32Encoded == encodeSonglengthsTsv(xxh32, _xxh32check))
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
  val encoded = encodeModInfosTsv(entries, _md5idx)
  val decoded = decodeModInfosTsv(encoded, idx2md5)
  val pretty = createPrettyModInfosTsv(decoded)

  val xxh32 = modinfosToXxh32(decoded)
  val xxh32Encoded = encodeModInfosTsv(xxh32, _xxh32idx)
  val xxh32Decoded = decodeModInfosTsv(xxh32Encoded, idx2xxh32)
  val xxh32Pretty = createPrettyModInfosTsv(xxh32)

  Files.write(Paths.get(s"$DEST/encoded/xxh32/modinfos.tsv"), xxh32Encoded.getBytes("UTF-8"))
  Files.write(Paths.get(s"$DEST/pretty/md5/modinfos.tsv"), pretty.getBytes("UTF-8"))
  Files.write(Paths.get(s"$DEST/pretty/xxh32/modinfos.tsv"), xxh32Pretty.getBytes("UTF-8"))

  assert(decoded == parsePrettyModInfosTsv(pretty))
  assert(encoded == encodeModInfosTsv(decoded, _md5idx))
  assert(xxh32Decoded == parsePrettyModInfosTsv(xxh32Pretty))
  assert(xxh32Encoded == encodeModInfosTsv(xxh32, _xxh32idx))
})

lazy val ampTsvs = Future(_try {
  val entries = amp.details.par.flatMap(detail =>
    detail.metas.groupBy(m => (m.md5, m.path)).map { case ((md5, path), m) =>
      var best = m.head
      if (m.size > 1) {
        best = m.maxBy(_.extra_authors.size)
      }
      best
    }.flatMap(m =>
      val path = m.path.substring(m.path.indexOf("/") + 1, m.path.lastIndexOf("/"))
      if (!(m.extra_authors.isEmpty && m.album.isEmpty)) {
        Some(MetaData(
          m.md5.take(12),
          amp.transformAuthors(m, detail),
          Buffer.empty,
          m.album,
          0,
          m._type,
          if (m.album.endsWith(" PC")) "PC" else "",
        ))
      } else None
    )).toBuffer.distinct

  ampdata = processMetaTsvs(entries, "amp.tsv")
})

lazy val modlandTsvs = Future(_try {
  val smus = sources.modland.filter(e => e.path.startsWith("IFF-SMUS/") && e.path.toLowerCase.endsWith(".smus"))
    .groupBy(_.path.split("/").take(3).mkString("/"))
  val entries = sources.modland.sortBy(_.md5).par.flatMap { e =>
    var path =
      if (e.path.startsWith("Ad Lib/")) e.path.substring("Ad Lib/".length)
      else e.path
    val format = path.substring(0, path.indexOf("/"))
    // XXX Ashley Hogg
    if (path.indexOf("/") == path.lastIndexOf("/")) {
      path = "_unknown"
    } else {
      path = path.substring(path.indexOf("/") + 1, path.lastIndexOf("/"))
    }
    if (path != "_unknown") {
      modland.parseModlandAuthorAlbum(format, path).flatMap { case (authors, album) =>
        var _album = album
          // XXX special IFF-SMUS album handling
        if (e.path.startsWith("IFF-SMUS/") && smus(e.path.split("/").take(3).mkString("/")).size <= 1 &&
            !Seq("Brian Howarth","Chris Grigg","Maggie").exists(a => authors.contains(a))) {
          _album = ""
        }
        if (!(authors.isEmpty && _album.isEmpty)) {
          Some(MetaData(
            e.md5.take(12),
            authors.sorted.distinct.toBuffer,
            Buffer.empty,
            _album,
            0
          ))
        } else None
      }
    } else None
  }.toBuffer.distinct

  modlanddata = processMetaTsvs(entries, "modland.tsv")
})

lazy val unexoticaTsvs = Future(_try {
  val entries = unexotica.metas.par.map { m =>
    val md5 = m._1
    val path = m._2
    val meta = m._4
    val authors = unexotica.transformAuthors(meta, path)
    val album = unexotica.transformAlbum(meta, path)
    val publishers = unexotica.transformPublishers(meta)
    val year = meta.year.fold(_.toString, _.toString)
    MetaData(
      md5.take(12),
      authors.sorted.distinct.toBuffer,
      publishers,
      album.trim,
      if (year != "Unknown") year.toInt else 0,
      meta.`type`,
      "Amiga",
    )
  }.toBuffer.distinct

  unexoticadata = processMetaTsvs(entries, "unexotica.tsv")
})

lazy val demozooTsvs = Future(_try {
  val entries = demozoo.metas.par.flatMap(demozoo.transformMeta).toBuffer.distinct
  demozoodata = processMetaTsvs(entries, "demozoo.tsv")
})

lazy val oldexoticaTsvs = Future(_try {
  val entries = oldexotica.metas.par.flatMap { m =>
    val _type = m.info.replaceAll("\\(.*\\)$","").trim
    val authors = oldexotica.transformAuthors(m, _type)
    val publishers = oldexotica.transformPublishers(m)
    val album = oldexotica.transformAlbum(m)
    val year = m.year.getOrElse(0)
    if (authors.isEmpty && publishers.isEmpty && album.isEmpty && year == 0) None
    else Some(MetaData(
      m.md5.take(12),
      authors,
      publishers,
      album,
      year,
      if (_type != "N/A" && _type != "?") _type else "",
      if (album.nonEmpty) if (m.info.contains("Falcon")) "Atari" else "Amiga" else "",
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
      m.year.getOrElse(0),
      m._type,
      m._platform,
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
      m.year.getOrElse(0),
      m._type,
      m._platform,
    ))
  }.toBuffer.distinct

  modsanthologydata = processMetaTsvs(entries, "modsanthology.tsv")
})

lazy val tosecmusicTsvs = Future(_try {
  val tosec = sources.tosecmusic ++ sources.tosecmusic_unknown
  val entries = tosec.sortBy(_.md5).distinct.par.flatMap { e =>
    tosecmusic.parseTosecMeta(e.md5, e.path).map { meta =>
      MetaData(
        e.md5.take(12),
        meta.authors,
        meta.publishers,
        meta.album,
        meta.year,
        meta._type,
        meta._platform,
      )
    }
  }.toBuffer.distinct

  // too unreliable, only used as secondary source
  // tosecmusicdata = processMetaTsvs(entries, "tosecmusic.tsv")
  tosecmusicdata = entries
})

lazy val fujiologyTsvs = Future(_try {
  val entries = fujiology.metas.par.flatMap { m =>
    if (m.authors.isEmpty && m.publishers.isEmpty && m.album.isEmpty && !m.year.isDefined) None
    else Some(MetaData(
      m.md5.take(12),
      m.authors,
      m.publishers,
      m.album,
      m.year.getOrElse(0),
      m.prodType,
      fujiology.normalizePlatform(m.system),
    ))
  }.toBuffer.distinct
  fujiologydata = processMetaTsvs(entries, "fujiology.tsv")
})

Seq(
  s"$DEST/encoded/xxh32",
  s"$DEST/pretty/md5",
  s"$DEST/pretty/xxh32"
).foreach { dir =>
  Files.createDirectories(Paths.get(dir))
}

// needs to be processed first
Await.ready(Future.sequence(Seq(md5idx,xxh32idxTsv)), Duration.Inf)

val future = Future.sequence(
  Seq(md5idx,
      xxh32idxTsv,
      songlengthsTsvs,
      modinfosTsvs,
      ampTsvs,
      modlandTsvs,
      unexoticaTsvs,
      demozooTsvs,
      oldexoticaTsvs,
      wantedteamTsvs,
      modsanthologyTsvs,
      fujiologyTsvs,
      tosecmusicTsvs,
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
      modsanthologydata,
      fujiologydata,
      tosecmusicdata,
      globalLeftovers.asScala.toBuffer
    )
    processMetaTsvs(combined, "metadata.tsv", true)
}

future onComplete {
  case Failure(e) =>
    e.printStackTrace()
    System.exit(1)
  case Success(value) =>
    System.out.println(s"Songdb files created to $DEST/")
}

Await.ready(future, Duration.Inf)
