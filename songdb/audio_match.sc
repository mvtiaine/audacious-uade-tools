#!/usr/bin/env -S scala-cli shebang --suppress-warning-directives-in-multiple-files -q

// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>
// NOTE: some code generated with Claude Sonnet 4

// Does a "brute force" search for matching audio fingerprints, based on chroma similarity scores.

// The fingerprint can be from short snippet with 3+ seconds or longer snippet of several minutes of audio.
// Results can depend on many factors, like audio quality and distinct audio features available.
// Usually at least 30 seconds of audio is needed to get non-random results, but the more the better.
// Depending on your system and input fingerprint length, running the script may take several minutes or more.

// grep the md5 from results to locate the matching mod files and all available metadata:
//    grep md5 sources/*.tsv
//    grep md5 ../tsv/pretty/md5/*.tsv

// NOTE: this requires a lot of memory (several GB), 8G+ recommended.
// NOTE: decompress the files in 'sources/audio' first with e.g.
//    zstd -d sources/audio/audio_*.zst

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0
//> using dep org.scodec::scodec-bits::1.2.4
//> using dep org.typelevel::spire::0.18.0

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
import scala.collection.parallel.CollectionConverters._
import scala.io.StdIn.readLine
import scala.util.boundary, boundary.break
import scala.collection.mutable.Buffer
import spire.math._

import audio._
import chromaprint._
import convert._
import pretty._
import sources._

val MINSCORE = 0.67
val MAXRESULTS = 10

if (args.length < 1) {
  Console.err.println("Usage:")
  Console.err.println(s"  ./audio_match.sc <input-chromaprint|-> [minscore (=$MINSCORE)] [maxresults (=$MAXRESULTS)]")
  Console.err.println()
  Console.err.println("Examples:")
  Console.err.println("  ./audio_match.sc AQAAC1EShUokRcMfoT-OX8RfNKHCG5V6iEue48cdHQAEEgYRCQhA0AAD")
  Console.err.println("  fpcalc -plain somefile.wav | ./audio_match.sc - 0.9 100")
  sys.exit(1)
}

if (Paths.get("sources/audio").toFile.listFiles.filter(_.getName.endsWith(".tsv")).isEmpty) {
  Console.err.println("Decompress the files in 'sources/audio' first with e.g.\nzstd -d sources/audio/audio_*.zst")
  sys.exit(1)
}

val input = args(0) 
val fingerprint = if (input == "-") {
  val line = readLine()
  if (line == null || line.isBlank()) {
    System.err.println("No input received from stdin")
    sys.exit(1)
  }
  line.trim()
} else input
val minscore = if (args.length >= 2) args(1).toDouble else MINSCORE
val maxresults = if (args.length >= 3) args(2).toInt else MAXRESULTS

val Right(algo,full) = FingerprintDecompressor(fingerprint) : @unchecked

System.err.print("Processing (x/16) ")
case class Result(md5: String, subsong: Int, score: Double)
var results = Buffer.empty[Result]
(0 to 15).foreach { i =>
  System.err.print(s".${i+1}.")
  val audioFingerprints = parseAudioTsv(Paths.get(s"sources/audio/audio_${i.toHexString}.tsv").toFile.getAbsolutePath, withSimHash = false)
  results ++= audioFingerprints.par.filter(_.audioChromaprint.nonEmpty).flatMap(af => {
    val Right(a,d) = FingerprintDecompressor(af.audioChromaprint) : @unchecked
    assert(a == algo)
    val score = chromaSimilarity(a, d, algo, full, 0.7)
    if (score >= minscore) {
      Some(Result(af.md5, af.subsong, score))
    } else None
  })
}
results = results.sortBy(_.score).reverse.take(maxresults)
System.err.print(" done.\n")

if (results.isEmpty) {
  System.err.println(s"No matches found with score >= $minscore")
  sys.exit(0)
} else {
  val metas = {
    val path = Paths.get("../tsv/pretty/md5/metadata.tsv")
    val tsv = Files.readString(path)
    parsePrettyMetaTsv(tsv).groupBy(_.hash)
  }
    
  val filenames = sources.tsvs.flatMap { case (source, entriesByMd5) =>
    entriesByMd5.flatMap { case (md5, entries) =>
      entries.filter(_.path.nonEmpty).map(entry => md5.take(12) -> entry.path.split('/').last)
    }
  }.groupBy(_._1).mapValues(_.map(_._2).sorted.distinct.mkString(", ")).toMap

  case class Column(header: String, maxWidth: Int, extract: (Result, Option[MetaData], Map[String, String]) => String)

  val columns = Seq(
    Column("Score", 6, (r, _, _) => r.score.formatted("%.3f")),
    Column("MD5", 12, (r, _, _) => r.md5),
    Column("Sub", 3, (r, _, _) => r.subsong.toString),
    Column("Authors", 30, (_, m, _) => m.map(_.authors.mkString(" & ")).getOrElse("")),
    Column("Album", 30, (_, m, _) => m.map(_.album).getOrElse("")),
    Column("Publishers", 30, (_, m, _) => m.map(_.publishers.mkString(" & ")).getOrElse("")),
    Column("Year", 4, (_, m, _) => m.map(y => if (y.year > 0) y.year.toString else "").getOrElse("")),
    Column("Filenames", 90, (r, _, s) => s.getOrElse(r.md5, ""))
  )

  val rows = results.map { r =>
    val metadata = metas.get(r.md5).map(_.head.asInstanceOf[MetaData])
    columns.map(_.extract(r, metadata, filenames))
  }

  val widths = columns.zipWithIndex.map { case (col, i) =>
    val dataWidth = rows.map(_(i).length).maxOption.getOrElse(0)
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
  rows.foreach(row => println(formatRow(row)))
  println()
}

def chromaSimilarity(
  algo1: Int,
  knownFullData: IndexedSeq[spire.math.UInt],
  algo2: Int,
  unknownData: IndexedSeq[spire.math.UInt],
  minMatchRatio: Double = 0.7
): Double = {
  if (algo1 != algo2) {
    return 0.0
  }
  if (knownFullData.isEmpty || unknownData.isEmpty) {
    return 0.0
  }

  if (unknownData.length > knownFullData.length * 1.2) {
    return 0.0
  }

  val unknownLength = unknownData.length
  val knownLength = knownFullData.length
  
  val minRequiredOverlap = math.max(math.min(unknownLength, 15), (unknownLength * minMatchRatio).toInt)
  
  var maxSimilarity = 0.0
  var bestOverlap = 0
  var bestOffset = 0

  val maxSearchOffset = knownLength - (unknownLength / 2)
  
  var offset = 0
  while (offset <= maxSearchOffset) {
    var currentScore = 0
    var overlap = 0

    val endIdx = math.min(unknownLength, knownLength - offset)
    
    var i = 0
    while (i < endIdx && offset + i < knownLength) {
      val j = i + offset
      val xorValue = unknownData(i) ^ knownFullData(j)
      currentScore += (32 - Integer.bitCount(xorValue.toInt))
      overlap += 1
      i += 1
    }
  
    if (overlap >= minRequiredOverlap) {
      val currentSimilarity = currentScore.toDouble / (overlap * 32.0)
      
      val coverageRatio = overlap.toDouble / unknownLength
      val coverageWeight = math.min(1.0, coverageRatio / minMatchRatio)
      val adjustedSimilarity = currentSimilarity * (0.7 + 0.3 * coverageWeight)
      
      if (adjustedSimilarity > maxSimilarity || 
          (math.abs(adjustedSimilarity - maxSimilarity) < 0.01 && overlap > bestOverlap)) {
        maxSimilarity = currentSimilarity
        bestOverlap = overlap
        bestOffset = offset
      }
    }

    offset += 1
  }

  val positionRatio = bestOffset.toDouble / knownLength
  val coverageRatio = bestOverlap.toDouble / unknownLength
  
  if (coverageRatio < minMatchRatio) {
    maxSimilarity *= (coverageRatio / minMatchRatio)
  }
  
  if (positionRatio > 0.95 && bestOverlap < unknownLength * 0.8) {
    maxSimilarity *= 0.95
  }

  maxSimilarity
}
