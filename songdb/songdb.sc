#!/usr/bin/env -S scala-cli shebang --suppress-warning-directives-in-multiple-files -q

// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using file scripts/md5.sc
//> using file scripts/dedup.sc
//> using file scripts/convert.sc
//> using file scripts/pretty.sc
//> using file scripts/combine.sc
//> using file scripts/xxh32.sc
//> using file scripts/chromaprint.sc

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
//> using file scripts/sources/audio.sc

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable
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
import xxh32._
import audio._
import chromaprint._
import tosecmusic._
import fujiology._

implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

// shutup warning
System.setProperty("log4j.provider", "org.apache.logging.log4j.simple.internal.SimpleProvider")
tosec.metas // force init

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

def dedupMeta(entries: Buffer[MetaData], name: String) = {
  val dedupped = entries.groupBy(_.hash).flatMap { case (hash, metas) =>
    if (metas.size > 1) {
      System.err.println(s"WARN: removing duplicate entries in ${name}, hash: ${metas.head.hash} entries: ${metas}")
    }
    val SORT = "\u0001"
    val meta = metas.sortBy(m => ("" +
     (if (m.year == 0) 9999 else m.year) + SORT +
     (if (m.authors.isEmpty) SEPARATOR else (10 - m.authors.size) + m.authors.mkString(SEPARATOR)) + SORT +
     (if (m.album.isEmpty) SEPARATOR else m.album) + SORT +
     (if (m.publishers.isEmpty) SEPARATOR else (10 - m.publishers.size) + m.publishers.mkString(SEPARATOR)) + SORT
    )).head
    if (meta.authors.isEmpty && meta.publishers.isEmpty && meta.album.isEmpty && meta.year == 0) {
      None
    } else {
      Some(MetaData(hash, meta.authors, meta.publishers, meta.album, meta.year, meta._type, meta._platform))
    }
  }.toBuffer
  dedupped
}

def processMetaTsvs(_entries: Buffer[MetaData], name: String, allTsvs: Boolean = false) = {
  val entries = dedupMeta(_entries, name)
  // encoding does also deduplication
  val encoded = encodeMetaTsv(entries, name, _md5idx)
  val decoded = decodeMetaTsv(encoded, idx2md5)
  val pretty = createPrettyMetaTsv(decoded)

  Files.write(Paths.get(s"/tmp/songdb/pretty/md5/${name}"), pretty.getBytes("UTF-8"))

  assert(decoded == parsePrettyMetaTsv(pretty))
  assert(encoded == encodeMetaTsv(decoded, name, _md5idx))

  if (allTsvs) {
    val xxh32 = metasToXxh32(decoded)
    val xxh32Encoded = encodeMetaTsv(xxh32, name + ".xxh32", _xxh32idx)
    val xxh32Decoded = decodeMetaTsv(xxh32Encoded, idx2xxh32)
    val xxh32Pretty = createPrettyMetaTsv(xxh32)

    Files.write(Paths.get(s"/tmp/songdb/pretty/xxh32/${name}"), xxh32Pretty.getBytes("UTF-8"))
    Files.write(Paths.get(s"/tmp/songdb/encoded/xxh32/${name}"), xxh32Encoded.getBytes("UTF-8"))

    assert(xxh32Decoded == parsePrettyMetaTsv(xxh32Pretty))
    assert(xxh32Encoded == encodeMetaTsv(xxh32, name + ".xxh32", _xxh32idx))
  }

  entries
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
  Files.write(Paths.get("/tmp/songdb/encoded/xxh32/xxh32idx.tsv"), encoded.getBytes("UTF-8"))
})

lazy val songlengthsTsvs = Future(_try {
  audio.audioByPlayerAndMd5 // XXX need explicit eager init before .par to avoid "unparallelized" init due to most threads waiting below
  val entries = songlengths.db.sortBy(_.md5).par.map(e => {
    val md5 = e.md5.take(12)
    val duplicates = mutable.SortedSet[Int]()
    val fingerprints = audio.audioByPlayerAndMd5.get((e.player, md5)).getOrElse(Buffer.empty)
    if (fingerprints.nonEmpty) {
      val filtered = fingerprints.filter(_.audioBytes > 0)
      val grouped = (
        if (filtered.forall(e => e.audioBytes > 2 * 11025 * 12 && e.audioBytes == filtered.head.audioBytes)) filtered.groupBy(_.audioBytes)
        else filtered.groupBy(_.audioTag.replaceFirst(s"^[0-9]+-", "")
      )).mapValues(_.distinct)
      if (!grouped.values.forall(group => group.map(_.subsong).distinct.size == group.size)) {
        System.err.println(s"WARN: inconsistent audio fingerprints for md5: $md5 player: ${e.player} format: ${e.format}")
      }
      assert(grouped.values.forall(group => group.map(_.subsong).sorted == group.map(_.subsong)))
      for ((_, group) <- grouped) {
        var remaining = group
        while (remaining.nonEmpty) {
          val cmp = remaining.head
          remaining = remaining.filterNot(_.subsong == cmp.subsong)
          for (e <- remaining) {
            var duplicate = true
            if (e.audioChromaprint != cmp.audioChromaprint) {       
              val threshold = if (filtered.forall(f => (f.audioTag == e.audioTag || f.audioBytes == e.audioBytes) && e.audioBytes > 2 * 11025 * 12)) 0.9 else 0.99
              val similarity = chromaSimilarity(cmp.audioChromaprint, e.audioChromaprint, threshold)
              if (similarity < threshold) {
                duplicate = false
                //System.err.println(s"DEBUG: Chromaprint similarity for ${md5} subsong ${cmp.subsong} vs ${e.subsong} is too low: ${similarity} threshold: ${threshold}")
              } else {
                //System.err.println(s"DEBUG: Chromaprint similarity for ${md5} subsong ${cmp.subsong} vs ${e.subsong}: ${similarity} threshold: ${threshold} (duplicate)")
              }
            } else if (e.audioHash != cmp.audioHash) {
              duplicate = false
              //System.err.println(s"DEBUG: Audio hash mismatch for ${md5} subsong ${cmp.subsong} vs ${e.subsong}: ${cmp.audioHash} vs ${e.audioHash}")
            }
            if (duplicate) {
              duplicates += e.subsong
            }
          }
          remaining = remaining.filterNot(e => duplicates.contains(e.subsong))
        }
      }
      if (duplicates.nonEmpty && e.subsongs.size > duplicates.size) {
        System.err.println(s"INFO: md5: $md5 has duplicate subsongs: ${duplicates.mkString(",")} player: ${e.player} format: ${e.format}")
      }
    }
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

  Files.write(Paths.get("/tmp/songdb/encoded/xxh32/songlengths.tsv"), xxh32Encoded.getBytes("UTF-8"))
  Files.write(Paths.get("/tmp/songdb/pretty/md5/songlengths.tsv"), pretty.getBytes("UTF-8"))
  Files.write(Paths.get("/tmp/songdb/pretty/xxh32/songlengths.tsv"), xxh32Pretty.getBytes("UTF-8"))

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

  Files.write(Paths.get("/tmp/songdb/encoded/xxh32/modinfos.tsv"), xxh32Encoded.getBytes("UTF-8"))
  Files.write(Paths.get("/tmp/songdb/pretty/md5/modinfos.tsv"), pretty.getBytes("UTF-8"))
  Files.write(Paths.get("/tmp/songdb/pretty/xxh32/modinfos.tsv"), xxh32Pretty.getBytes("UTF-8"))

  assert(decoded == parsePrettyModInfosTsv(pretty))
  assert(encoded == encodeModInfosTsv(decoded, _md5idx))
  assert(xxh32Decoded == parsePrettyModInfosTsv(xxh32Pretty))
  assert(xxh32Encoded == encodeModInfosTsv(xxh32, _xxh32idx))
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
        0,
        m._type,
        if (m.album.endsWith(" PC")) "PC" else "",
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
      modland.parseModlandAuthorAlbum(format, path).map { case (authors, album) =>
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
      if (year != "Unknown") year.toInt else 0,
      meta.`type`,
      "Amiga",
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
      hash = md5.take(12),
      authors = if (authors.forall(_.trim.isEmpty)) Buffer.empty else authors,
      publishers = ((m.prodPublishers, m.party, m.modPublishers) match {
        case (prod,_,_) if useProd =>
          if (prod.forall(_.trim.isEmpty)) Buffer.empty else prod.toBuffer
        case (_,party,_) if !party.isEmpty =>
          Buffer(party.get)
        case (_,_,mod) if !mod.isEmpty =>
          if (mod.forall(_.trim.isEmpty)) Buffer.empty else mod.toBuffer
        case _ => Buffer.empty
      }).sorted,
      album = if (useProd) m.prod.trim else "",
      year = if (!earliestDate.isEmpty) earliestDate.substring(0,4).toInt else 0,
      _type = m.prodType.getOrElse(""),
      _platform = if (m.prodPlatforms.isEmpty || m.prodPlatforms.size > 1) "" else demozoo.normalizePlatform(m.prodPlatforms.head)
      //if (!m.prodPlatforms.isEmpty) m.prodPlatforms else m.modPlatform
    )
    info match {
      case MetaData(_, Buffer(), Buffer(), "", 0, "", "") => None
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
    val _type = m.info.replaceAll("\\(.*\\)$","").trim
    if (authors.isEmpty && publishers.isEmpty && album.isEmpty && year == 0) None
    else Some(MetaData(
      m.md5.take(12),
      authors,
      publishers,
      album,
      year,
      if (_type != "N/A") _type else "",
      "Amiga",
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
      m.year.getOrElse(0),
      m._type,
      "Amiga",
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
        meta._type
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
  "/tmp/songdb/encoded/xxh32",
  "/tmp/songdb/pretty/md5",
  "/tmp/songdb/pretty/xxh32"
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
    )
    processMetaTsvs(combined, "metadata.tsv", true)
}

future onComplete {
  case Failure(e) =>
    e.printStackTrace()
    System.exit(1)
  case Success(value) =>
    System.out.println("Songdb files created to /tmp/songdb/")
}

Await.ready(future, Duration.Inf)
