#!/usr/bin/env -S scala-cli shebang --jvm 25 -S 3.8 --suppress-warning-directives-in-multiple-files -q -J --sun-misc-unsafe-memory-access=allow -J -Xmx8G

// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2026 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

//> using file scripts/chromaprint.sc
//> using file scripts/convert.sc
//> using file scripts/dedup.sc
//> using file scripts/md5.sc
//> using file scripts/pretty.sc
//> using file scripts/songlengths.sc
//> using file scripts/sources/audio.sc
//> using file scripts/sources/sources.sc

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
val MAXRESULTS = 20

if (args.length < 1) {
  Console.err.println("Usage:")
  Console.err.println(s"  ./find_dupes.sc <input-file> [minscore (=$MINSCORE)] [maxresults (=$MAXRESULTS)]")
  Console.err.println()
  sys.exit(1)
}

if (Paths.get("sources/audio").toFile.listFiles.filter(_.getName.endsWith(".tsv")).isEmpty) {
  Console.err.println("Decompress the files in 'sources/audio' first with e.g.\nzstd -d sources/audio/audio_*.zst")
  sys.exit(1)
}

val input = args(0) 
val minscore = if (args.length >= 2) args(1).toDouble else MINSCORE
val maxresults = if (args.length >= 3) args(2).toInt else MAXRESULTS

val file = Paths.get(input)
if (!file.toFile.exists) {
  Console.err.println(s"Input file '${input}' does not exist")
  sys.exit(1)
}

val md5 = _md5(Files.readAllBytes(file)).map("%02x".format(_)).mkString.take(12)

val fingerprints = parseAudioTsv(Paths.get(s"sources/audio/audio_${md5.take(1)}.tsv").toFile.getAbsolutePath, withSimHash = false, md5s = Set(md5))

if (fingerprints.isEmpty) {
  Console.err.println(s"No fingerprints found for md5 ${md5}")
  sys.exit(1)
}

System.err.print("Processing (x/16) ")

val n = AtomicInteger(0)
final case class Result(md5: String, subsong: Int, score: Double)
var results = (0 to 15).par.flatMap { i =>
  val cmpFingerprints = parseAudioTsv(Paths.get(s"sources/audio/audio_${i.toHexString}.tsv").toFile.getAbsolutePath, withSimHash = false, lengths = fingerprints.map(_.audioBytes).toSet)
    .filterNot(_.md5 == md5)
  val results = cmpFingerprints.flatMap(af => {
    if (fingerprints.exists(f => f.audioHash == af.audioHash)) {
      Some(Result(af.md5, af.subsong, 1.0))
    } else if (af.audioChromaprint.nonEmpty) {
      fingerprints.filter(_.audioChromaprint.nonEmpty).flatMap(f => {
        val score = chromaSimilarity(f.audioChromaprint, af.audioChromaprint)
        if (score >= minscore) {
          Some(Result(af.md5, af.subsong, score))
        } else None
      })
    } else None
  })
  System.err.print(s".${n.incrementAndGet()}.")
  results
}.seq
results = results.sortBy(_.score).reverse.distinct.take(maxresults)
val resultMd5s = results.map(_.md5).toSet

System.err.print(" done.\n")

if (results.isEmpty) {
  System.err.println(s"No matches found with score >= $minscore")
  sys.exit(0)
} else {
  val metas = {
    val path = Paths.get("../tsv/pretty/md5/metadata.tsv")
    val tsv = Files.readString(path)
    parsePrettyMetaTsv(tsv).par.groupBy(_.hash)
  }

  final case class FileInfo(format: String, filesize: Int, filename: String, source: String)
  val fileinfos = sources.tsvs.par.flatMap { case (source, entriesByMd5) =>
    entriesByMd5.par.filter(e => resultMd5s.contains(e._1.take(12)) || e._1.take(12) == md5).flatMap { case (md5, entries) =>
      entries
        .filterNot(_.path.isEmpty)
        .map(entry =>
          md5.take(12) -> FileInfo(
            entry.format,
            entry.filesize,
            if (source == Source.SOAMC && entry.path.startsWith("001/")) "" else entry.path.split('/').last,
            source.toString
          )
        )
    }
  }.groupBy(_._1).mapValues(_.map(_._2).seq.toSeq.distinct).toMap.seq
  
  final case class Column(header: String, maxWidth: Int, extract: (Result, Option[MetaData], Map[String, Seq[FileInfo]]) => String)

  val columns = Seq(
    Column("Score", 6, (r, _, _) => r.score.formatted("%.3f")),
    Column("MD5", 12, (r, _, _) => r.md5),
    Column("Size", 9, (r, _, fi) => fi(r.md5).head.filesize.toString),
    Column("Format", 30, (r, _, fi) => fi(r.md5).map(_.format).sorted.head),
    Column("Sub", 3, (r, _, _) => (if (r.subsong >= 0) r.subsong.toString else "*")),
    Column("Filenames", 30, (r, _, fi) => fi(r.md5).map(_.filename).filterNot(_.isEmpty).sorted.distinct.mkString(", ")),
    Column("#", 3, (r, _, fi) => fi(r.md5).map(_.source).sorted.distinct.length.toString),
    Column("Authors", 30, (_, m, _) => m.map(_.authors.mkString(" & ")).getOrElse("")),
    Column("Album", 30, (_, m, _) => m.map(_.album).getOrElse("")),
    Column("Publishers", 30, (_, m, _) => m.map(_.publishers.mkString(" & ")).getOrElse("")),
    Column("Year", 4, (_, m, _) => m.map(y => if (y.year > 0) y.year.toString else "").getOrElse("")),
  )

  val cmp = {
    val metadata = metas.get(md5).map(_.head.asInstanceOf[MetaData])
    columns.map(_.extract(Result(md5, -1, 1.0), metadata, fileinfos))
  }

  val rows = results.map { r =>
    val metadata = metas.get(r.md5).map(_.head.asInstanceOf[MetaData])
    columns.map(_.extract(r, metadata, fileinfos))
  }

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
  println()
}
