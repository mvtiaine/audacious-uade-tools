#!/usr/bin/env -S scala-cli shebang --suppress-warning-directives-in-multiple-files -q

// SPDX-License-Identifier: GPL-2.0-or-later AND CC-PDM-1.0
// SPDX-AI-Disclosure: ai-assisted
// Copyright (C) 2026 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using jvm 27
//> using scala 3.9
//> using option -opt
//> using option -opt-inline:<sources>
//> using javaOpt -Xmx8G
//> using javaOpt --sun-misc-unsafe-memory-access=allow
//> using javaOpt --enable-native-access=ALL-UNNAMED
//> using javaOpt -XX:+UseCompactObjectHeaders
//> using javaOpt -XX:+UseCompressedOops
//> using javaOpt -XX:MetaspaceSize=256m

// Input is a mod file, its 12/32-char md5, or '-' for stdin; all subsongs of it are
// compared against every fingerprinted subsong in the songdb.
// Output is a human readable table by default, TSV with --tsv.
// --all: one row per matching file path (with its source name) instead of an
// aggregated filename list and source count.

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

//> using file scripts/LockFreeMaps.scala
//> using file scripts/TsvReader.scala
//> using file scripts/chromaprint.sc
//> using file scripts/convert.sc
//> using file scripts/dedup.sc
//> using file scripts/md5.sc
//> using file scripts/normalization.sc
//> using file scripts/pretty.sc
//> using file scripts/songlengths.sc
//> using file scripts/threadpools.sc
//> using file scripts/sources/audio.sc
//> using file scripts/sources/sources.sc

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicInteger
import java.security.MessageDigest
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._

import audio._
import chromaprint._
import convert._
import pretty._
import sources._

def _md5(b: Array[Byte]) = {
    MessageDigest.getInstance("MD5").digest(b)
}

val MINSCORE = 0.67
val MAXRESULTS = 30
val MAXLENDIFF = 10.0

val tsv = args.contains("--tsv")
val all = args.contains("--all")
val positional = args.filterNot(a => a == "--tsv" || a == "--all")

if (positional.length < 1) {
  Console.err.println("Usage:")
  Console.err.println(s"  ./find_dupes.sc [--tsv] [--all] <input-file|md5|-> [minscore (=$MINSCORE)] [maxresults (=$MAXRESULTS)] [maxlen-diff (=$MAXLENDIFF)]")
  Console.err.println()
  Console.err.println("  --tsv  tab separated output instead of a human readable table")
  Console.err.println("  --all  one row per matching file path (with its source) instead of")
  Console.err.println("         an aggregated filename list and source count")
  Console.err.println("  --tsv/--all default to unlimited results unless maxresults is given")
  Console.err.println("  maxlen-diff  maximum length difference in seconds when searching candidates")
  Console.err.println()
  Console.err.println("Examples:")
  Console.err.println("  ./find_dupes.sc somefile.mod")
  Console.err.println("  ./find_dupes.sc --all --tsv 00000b104a70 0.9 100")
  sys.exit(1)
}

if (Paths.get("sources/audio").toFile.listFiles.filter(_.getName.endsWith(".tsv")).isEmpty) {
  Console.err.println("Decompress the files in 'sources/audio' first with e.g.\nzstd -d sources/audio/audio_*.zst")
  sys.exit(1)
}

val input = positional(0)
val md5 = if (input == "-") {
  _md5(LazyList.continually(System.in.read).takeWhile(_ != -1).map(_.toByte).toArray).map("%02x".format(_)).mkString
} else if (input.matches("[0-9a-fA-F]{32}|[0-9a-fA-F]{12}")) {
  input.toLowerCase
} else {
  val file = Paths.get(input)
  if (!file.toFile.exists) {
    Console.err.println(s"Input file '${input}' does not exist")
    sys.exit(1)
  }
  _md5(Files.readAllBytes(file)).map("%02x".format(_)).mkString
}
val hash = md5.take(12)

val minscore = if (positional.length >= 2) positional(1).toDouble else MINSCORE
// unlimited results unless explicitly given with --all or --tsv
val maxresults = if (positional.length >= 3) positional(2).toInt else if (all || tsv) Int.MaxValue else MAXRESULTS
val maxlenDiff = if (positional.length >= 4) positional(3).toDouble else MAXLENDIFF

val fingerprints = parseAudioTsv(Paths.get(s"sources/audio/audio_${hash.take(1)}.tsv").toFile.getAbsolutePath, withSimHash = false, md5s = Set(hash))

val n = AtomicInteger(0)
final case class Result(md5: String, subsong: Int, score: Double, audioBytes: Int)
val inputFPs = fingerprints.filter(_.audioChromaprint.nonEmpty).map(f => (f, decodeChromaprintUncached(f.audioChromaprint))).toBuffer
if (fingerprints.nonEmpty) System.err.print("Processing (x/16) ")
var results = if (fingerprints.isEmpty) Seq.empty[Result] else (0 to 15).par.flatMap { i =>
  val cmpFingerprints = parseAudioTsv(Paths.get(s"sources/audio/audio_${i.toHexString}.tsv").toFile.getAbsolutePath, withSimHash = false, lengths = fingerprints.map(_.audioBytes).toSet, lengthTolerance = maxlenDiff)
    .filterNot(_.md5 == hash)
  val results = cmpFingerprints.flatMap(af => {
    if (fingerprints.exists(f => f.audioHash == af.audioHash)) {
      Some(Result(af.md5, af.subsong, 1.0, af.audioBytes))
    } else if (af.audioChromaprint.nonEmpty) {
      val afp = decodeChromaprintUncached(af.audioChromaprint)
      inputFPs.flatMap { case (f, fp) =>
        val score = chromaSimilarityFPs(fp, afp)
        if (score >= minscore) {
          Some(Result(af.md5, af.subsong, score, af.audioBytes))
        } else None
      }
    } else None
  })
  System.err.print(s".${n.incrementAndGet()}.")
  results
}.seq
results = results.sortBy(_.score).reverse.distinct
val moreResults = results.size - maxresults
results = results.take(maxresults)
val resultMd5s = results.map(_.md5).toSet

if (fingerprints.nonEmpty) System.err.print(" done.\n")

if (results.isEmpty) {
  val inDb = sources.tsvs.exists(_._2.exists(_._1.take(12) == hash))
  if (!inDb) {
    Console.err.println(s"md5 ${hash} not found in the database")
    sys.exit(1)
  }
}

val metas = parsePrettyMetaTsv(Files.readString(Paths.get("../tsv/pretty/md5/metadata.tsv"), StandardCharsets.UTF_8)).par.groupBy(_.hash).seq

def lenStr(audioBytes: Int): String =
  if (audioBytes <= 0) "" else "%02d:%02d".format(audioBytes / persecondbytes / 60, audioBytes / persecondbytes % 60)

if (all) {
  // every file path of each matching hash, with its source name, size, format, player and channels
  val fileinfos = sources.tsvs.par.flatMap { case (source, entriesByMd5) =>
    entriesByMd5.par.filter(e => resultMd5s.contains(e._1.take(12)) || e._1.take(12) == hash).flatMap { case (md5, entries) =>
      entries.filterNot(_.path.isEmpty).map(entry => (md5.take(12), source.toString, entry.path, entry.filesize, entry.format, entry.player, entry.channels))
    }
  }.seq.groupBy(_._1).map { case (h, infos) => h -> infos.map(t => (t._2, t._3, t._4, t._5, t._6, t._7)).distinct.sorted }.toMap

  final case class Row(score: Double, hash: String, filesize: Int, format: String, player: String, subsong: Int, audioBytes: Int, channels: Int, source: String, path: String) {
    def meta: Option[MetaData] = metas.get(hash).map(_.head)
  }

  val inputAudioBytes = fingerprints.map(_.audioBytes).maxOption.getOrElse(0)
  // input first, then matches by descending score; one row per source+path
  val rows = Buffer[Row]()
  fileinfos.getOrElse(hash, Seq(("", "", -1, "", "", 0))).foreach { case (source, path, filesize, format, player, channels) =>
    rows += Row(1.0, hash, filesize, format, player, -1, inputAudioBytes, channels, source, path)
  }
  results.sortBy(r => (-r.score, r.md5)).foreach { r =>
    fileinfos.getOrElse(r.md5, Seq(("", "", -1, "", "", 0))).foreach { case (source, path, filesize, format, player, channels) =>
      rows += Row(r.score, r.md5, filesize, format, player, r.subsong, r.audioBytes, channels, source, path)
    }
  }

  def cols(r: Row): Seq[String] = Seq(
    "%.3f".format(r.score),
    r.hash,
    if (r.filesize >= 0) r.filesize.toString else "",
    r.format,
    r.player,
    if (r.subsong >= 0) r.subsong.toString else "*",
    lenStr(r.audioBytes),
    if (r.channels > 0) r.channels.toString else "",
    r.source,
    r.path,
    r.meta.map(_.authors.mkString(" & ")).getOrElse(""),
    r.meta.map(_.album).getOrElse(""),
    r.meta.map(_.publishers.mkString(" & ")).getOrElse(""),
    r.meta.map(m => if (m.year > 0) m.year.toString else "").getOrElse(""),
  )

  val headers = Seq(
    "Score", "MD5", "Size", "Format", "Player", "Sub", "Len", "Ch", "Source", "Path",
    "Authors", "Album", "Publishers", "Year",
  )

  if (tsv) {
    println(headers.mkString("\t"))
    rows.foreach(r => println(cols(r).mkString("\t")))
  } else {
    val maxwidths = Seq(6, 12, 9, 25, 12, 3, 6, 2, 20, 55, 25, 25, 25, 4)

    val widths = headers.zip(maxwidths).zipWithIndex.map { case ((header, maxWidth), i) =>
      val dataWidth = rows.map(r => cols(r)(i).length).maxOption.getOrElse(0)
      math.min(maxWidth, math.max(header.length, dataWidth))
    }

    def truncate(text: String, width: Int): String = {
      if (text.length <= width) text else text.take(width - 1) + "…"
    }

    // paths show the tail of the path, keeping the filename's end visible
    def truncatePath(text: String, width: Int): String =
      if (text.length <= width) text else "…" + text.takeRight(width - 1)

    val pathIndex = headers.indexOf("Path")

    def formatRow(values: Seq[String]): String = {
      values.zip(widths).zipWithIndex.map { case ((value, width), i) =>
        val truncated = if (i == pathIndex) truncatePath(value, width) else truncate(value, width)
        truncated.padTo(width, ' ')
      }.mkString(" | ")
    }

    println()
    println(formatRow(headers))
    println("-" * formatRow(headers).length)
    val separator = "-" * formatRow(headers).length
    val inputRows = rows.indexWhere(_.hash != hash)
    rows.zipWithIndex.foreach { case (r, i) =>
      if (i == inputRows) println(separator)
      println(formatRow(cols(r)))
    }
    if (moreResults > 0) {
      println(s"... $moreResults more")
    }
    println()
  }
}

if (!all) {
  final case class FileInfo(format: String, player: String, filesize: Int, filename: String, channels: Int, source: String)
  val fileinfos = sources.tsvs.par.flatMap { case (source, entriesByMd5) =>
    entriesByMd5.par.filter(e => resultMd5s.contains(e._1.take(12)) || e._1.take(12) == hash).flatMap { case (md5, entries) =>
      entries
        .filterNot(_.path.isEmpty)
        .map(entry =>
          md5.take(12) -> FileInfo(
            entry.format,
            entry.player,
            entry.filesize,
            if (source == Source.SOAMC && entry.path.startsWith("001/")) "" else entry.path.split('/').last,
            entry.channels,
            source.toString
          )
        )
    }
  }.groupBy(_._1).mapValues(_.map(_._2).seq.toSeq.distinct).toMap.seq

  final case class Column(header: String, maxWidth: Int, extract: (Result, Option[MetaData], Map[String, Seq[FileInfo]]) => String)

  val columns = Seq(
    Column("Score", 6, (r, _, _) => "%.3f".format(r.score)),
    Column("MD5", 12, (r, _, _) => r.md5),
    Column("Size", 9, (r, _, fi) => fi(r.md5).head.filesize.toString),
    Column("Format", 30, (r, _, fi) => fi(r.md5).map(_.format).sorted.head),
    Column("Player", 12, (r, _, fi) => fi(r.md5).map(_.player).filterNot(_.isEmpty).sorted.distinct.mkString(", ")),
    Column("Sub", 3, (r, _, _) => (if (r.subsong >= 0) r.subsong.toString else "*")),
    Column("Len", 6, (r, _, _) => lenStr(r.audioBytes)),
    Column("Ch", 2, (r, _, fi) => fi(r.md5).map(_.channels).filter(_ > 0).distinct.sorted.mkString(", ")),
    Column("Filenames", 30, (r, _, fi) => fi(r.md5).map(_.filename).filterNot(_.isEmpty).sorted.distinct.mkString(", ")),
    Column("#", 3, (r, _, fi) => fi(r.md5).map(_.source).sorted.distinct.length.toString),
    Column("Authors", 30, (_, m, _) => m.map(_.authors.mkString(" & ")).getOrElse("")),
    Column("Album", 30, (_, m, _) => m.map(_.album).getOrElse("")),
    Column("Publishers", 30, (_, m, _) => m.map(_.publishers.mkString(" & ")).getOrElse("")),
    Column("Year", 4, (_, m, _) => m.map(y => if (y.year > 0) y.year.toString else "").getOrElse("")),
  )

  val cmp = {
    val metadata = metas.get(hash).map(_.head.asInstanceOf[MetaData])
    columns.map(_.extract(Result(hash, -1, 1.0, fingerprints.map(_.audioBytes).max), metadata, fileinfos))
  }

  val rows = results.map { r =>
    val metadata = metas.get(r.md5).map(_.head.asInstanceOf[MetaData])
    columns.map(_.extract(r, metadata, fileinfos))
  }

  if (tsv) {
    println(columns.map(_.header).mkString("\t"))
    println(cmp.mkString("\t"))
    rows.foreach(row => println(row.mkString("\t")))
  } else {
    val widths = columns.zipWithIndex.map { case (col, i) =>
      val dataWidth = (cmp +: rows).map(_(i).length).maxOption.getOrElse(0)
      math.min(col.maxWidth, math.max(col.header.length, dataWidth))
    }

    def truncate(text: String, width: Int): String = {
      if (text.length <= width) text else text.take(width - 1) + "…"
    }

    def formatRow(values: Seq[String]): String = {
      values.zip(widths).map { case (value, width) =>
        truncate(value, width).padTo(width, ' ')
      }.mkString(" | ")
    }

    println()
    println(formatRow(columns.map(_.header)))
    println("-" * formatRow(columns.map(_.header)).length)
    println(formatRow(cmp))
    println("-" * formatRow(columns.map(_.header)).length)
    rows.foreach(row => println(formatRow(row)))
    if (moreResults > 0) {
      println(s"... $moreResults more")
    }
    println()
  }
}
