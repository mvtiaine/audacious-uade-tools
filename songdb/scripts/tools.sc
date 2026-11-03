// SPDX-License-Identifier: CC-PDM-1.0
// SPDX-AI-Disclosure: ai-generated

// Shared helpers for source_metas.sc, find_dupes.sc and audio_match.sc.
//
// Human readable table output: aligned by display columns, so a combining mark
// adds none and an East Asian wide character two. The text itself is never
// altered, so paths stay copy/pasteable.

//> using dep com.ibm.icu:icu4j:78.3
//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import com.ibm.icu.lang.{UCharacter, UProperty}
import com.ibm.icu.text.BreakIterator
import java.nio.file.Paths
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._

// the decompressed audio shards, sorted by name; exits with the zstd hint when
// they are missing
def audioShardFiles(): Array[java.io.File] =
  val shards = Paths.get("sources/audio").toFile.listFiles.toSeq
    .filter(_.getName.endsWith(".tsv")).sortBy(_.getName)
  if (shards.isEmpty) {
    Console.err.println("Decompress the files in 'sources/audio' first with e.g.\nzstd -d sources/audio/audio_*.zst")
    sys.exit(1)
  }
  shards.toArray

// a fingerprint match of one subsong
final case class Result(md5: String, subsong: Int, score: Double, audioBytes: Int)

// one file of a md5 in the source tsvs; filename is the path's last segment, empty
// for SOAMC 001/ entries (duplicated/corrupted entries with random filenames)
final case class FileInfo(format: String, player: String, filesize: Int, filename: String, channels: Int, source: String)

def fileInfosFor(md5s: Set[String]): Map[String, Seq[FileInfo]] =
  sources.tsvs.par.flatMap { case (source, entriesByMd5) =>
    entriesByMd5.par.filter(e => md5s.contains(e._1.take(12))).flatMap { case (md5, entries) =>
      entries
        .filterNot(_.path.isEmpty)
        .map(entry =>
          md5.take(12) -> FileInfo(
            entry.format,
            entry.player,
            entry.filesize,
            if (source == sources.Source.SOAMC && entry.path.startsWith("001/")) "" else entry.path.split('/').last,
            entry.channels,
            source.toString
          )
        )
    }
  }.groupBy(_._1).mapValues(_.map(_._2).seq.toSeq.distinct).toMap.seq

// grapheme clusters with their display width (1 or 2 columns)
def clusters(s: String): Array[(String, Int)] =
  val bi = BreakIterator.getCharacterInstance()
  bi.setText(s)
  val out = Buffer.empty[(String, Int)]
  var start = bi.first()
  var end = bi.next()
  while (end != BreakIterator.DONE) {
    val g = s.substring(start, end)
    val eaw = UCharacter.getIntPropertyValue(g.codePointAt(0), UProperty.EAST_ASIAN_WIDTH)
    out += g -> (if eaw == UCharacter.EastAsianWidth.WIDE || eaw == UCharacter.EastAsianWidth.FULLWIDTH then 2 else 1)
    start = end
    end = bi.next()
  }
  out.toArray

// plain ASCII needs no iteration: one column per char
def displayWidth(s: String): Int =
  if (s.forall(_ <= '\u007f')) s.length
  else clusters(s).iterator.map(_._2).sum

// the leading/trailing clusters fitting in w columns
private def take(cs: Array[(String, Int)], w: Int): Array[(String, Int)] =
  var i = 0
  var left = w
  while (i < cs.length && cs(i)._2 <= left) { left -= cs(i)._2; i += 1 }
  cs.take(i)

private def takeRight(cs: Array[(String, Int)], w: Int): Array[(String, Int)] =
  var i = cs.length
  var left = w
  while (i > 0 && cs(i - 1)._2 <= left) { left -= cs(i - 1)._2; i -= 1 }
  cs.drop(i)

// the text cut down to w columns and its width; middle keeps both ends, eliding
// the middle with the marker (for paths), otherwise it truncates with a trailing
// ellipsis
def fit(s: String, w: Int, middle: Boolean, marker: String = " ⋯ "): (String, Int) =
  val width = displayWidth(s)
  if (width <= w) (s, width)
  else
    val cs = clusters(s)
    if (middle)
      val mw = displayWidth(marker)
      val head = (w - mw) / 2
      val cut = take(cs, head) ++ Array(marker -> mw) ++ takeRight(cs, w - mw - head)
      (cut.iterator.map(_._1).mkString, cut.iterator.map(_._2).sum)
    else
      val cut = take(cs, w - 1)
      (cut.iterator.map(_._1).mkString + "…", cut.iterator.map(_._2).sum + 1)

// a table column: header, max width, and whether it elides in the middle (paths)
final case class Col(header: String, maxWidth: Int, middle: Boolean = false)

// a table over the given columns and data rows: column widths are the widest of
// the header and the rows (display width), capped at maxWidth. `print` emits the
// header, a rule, and the rows, with an extra rule before each index in
// `separators` and the footer last; `formatRow`/`rule` are exposed for custom
// layouts (eg. group separators between fingerprint match groups).
final class Table(val columns: Seq[Col], val rows: Seq[Seq[String]], middleMarker: String = " ⋯ ") {
  val widths: Seq[Int] = columns.zipWithIndex.map { case (col, i) =>
    val dataWidth = rows.map(r => displayWidth(r(i))).maxOption.getOrElse(0)
    math.min(col.maxWidth, math.max(displayWidth(col.header), dataWidth))
  }
  val rule: String = "-" * (widths.sum + 3 * (widths.length - 1))
  def formatRow(values: Seq[String]): String =
    values.zip(columns).zip(widths).map { case ((value, col), width) =>
      val (text, used) = fit(value, width, col.middle, middleMarker)
      text + " " * (width - used)
    }.mkString(" | ")
  def print(separators: Set[Int] = Set.empty, footer: String = ""): Unit = {
    println()
    println(formatRow(columns.map(_.header)))
    println(rule)
    rows.zipWithIndex.foreach { case (r, i) =>
      if (separators.contains(i)) println(rule)
      println(formatRow(r))
    }
    if (footer.nonEmpty) println(footer)
    println()
  }
  def printTsv(): Unit = {
    println(columns.map(_.header).mkString("\t"))
    rows.foreach(r => println(r.mkString("\t")))
  }
}
