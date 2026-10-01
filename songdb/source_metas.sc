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

// MD5 based metadata and infos from songdb for one (or more) source(s) or directory(s).
// Human readable table to stdout by default, TSV with --tsv.
// No audio fingerprinting: pure md5 lookups only.

// NOTE: running this first time will fetch/install dependencies etc. which may take a while.

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

//> using file scripts/TsvReader.scala
//> using file scripts/convert.sc
//> using file scripts/dedup.sc
//> using file scripts/md5.sc
//> using file scripts/normalization.sc
//> using file scripts/pretty.sc
//> using file scripts/sources/sources.sc

import java.io.{File, FileInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._

import convert._
import pretty._
import sources._

val tsv = args.contains("--tsv")
val unique = args.contains("--unique")
val positional = args.filterNot(a => a == "--tsv" || a == "--unique")

if (positional.length < 1) {
  Console.err.println("Usage:")
  Console.err.println("  ./source_metas.sc [--tsv] [--unique] <source|all|directory> [source|directory...]")
  Console.err.println()
  Console.err.println("  --unique  only print md5s found in exactly one source")
  Console.err.println("  all     all sources")
  Console.err.println()
  Console.err.println("  ./source_metas.sc --tsv Deck > deck.tsv")
  Console.err.println("  ./source_metas.sc --unique ~/mods")
  Console.err.println("  ./source_metas.sc Modland NetlabelArchive")
  Console.err.println()
  Console.err.println(s"Sources: ${tsvfiles.map(_._2).distinct.filter(_ != Source.NONE).map(_.toString).sorted.mkString(", ")}")
  sys.exit(1)
}

sealed trait Target
final case class SourceTarget(source: Source) extends Target
final case class DirTarget(dir: File) extends Target

val targets: Seq[Target] = positional.toSeq.flatMap { arg =>
  if (arg.equalsIgnoreCase("all")) tsvfiles.map(_._2).distinct.filter(_ != Source.NONE).toSeq.map(SourceTarget(_))
  else {
    val f = new File(arg)
    if (f.isDirectory) Seq(DirTarget(f))
    else Seq(Source.values.find(_.toString.equalsIgnoreCase(arg.stripSuffix(".tsv"))).map(SourceTarget(_)).getOrElse {
      Console.err.println(s"Unknown source '${arg}'")
      sys.exit(1)
    })
  }
}.distinct

// source name shown in the Source column; directories use their name, not the full path
def targetName(t: Target): String = t match {
  case SourceTarget(s) => s.toString
  case DirTarget(d) => d.getName
}

val multi = targets.size > 1

// Parse only the requested sources' tsvs (see sources.tsvs for the full set).
def parseSourceTsv(file: String): Map[String, Buffer[TsvEntry]] = {
  val out = Buffer[TsvEntry]()
  var player = ""
  TsvReader.foreach(new java.io.File(s"sources/${file}")) { line =>
    if (line.fields > 4) {
      player = line.field(4)
      out += TsvEntry(line.field(0), line.int(1), line.int(2), line.field(3), player, line.field(5), if (line.field(6).isEmpty) 0 else line.int(6), line.int(7), line.field(8), line.field(9), line.field(10))
    } else out += TsvEntry(line.field(0), line.int(1), line.int(2), line.field(3), player, "", 0, -1, "", "", "")
  }
  out.groupBy(_.md5)
}

def lenStr(songlength: Int): String =
  if (songlength <= 0) "" else "%02d:%02d".format(songlength / 1000 / 60, songlength / 1000 % 60)

def readPrettyTsv(relative: String): String = {
  val path = Paths.get(s"../tsv/pretty/md5/${relative}")
  if (!path.toFile.exists) {
    Console.err.println(s"'${path}' does not exist")
    sys.exit(1)
  }
  Files.readString(path, StandardCharsets.UTF_8)
}

val metas = parsePrettyMetaTsv(readPrettyTsv("metadata.tsv")).par.groupBy(_.hash).seq

// --unique: a md5 is unique when it occurs in exactly one source
lazy val hashOwners = {
  val owners = new ConcurrentHashMap[String, Source]()
  val shared = ConcurrentHashMap.newKeySet[String]()
  sources.tsvs.par.foreach { case (source, entries) =>
    entries.keys.map(_.take(12)).toSet.foreach { h =>
      val prev = owners.putIfAbsent(h, source)
      if (prev != null && prev != source) shared.add(h)
    }
  }
  (owners, shared)
}

// unique among sources: exactly one source has it, and it is this one
def isUnique(hash: String, source: Source): Boolean =
  !hashOwners._2.contains(hash) && hashOwners._1.get(hash) == source

// directory mode only
lazy val modinfos = parsePrettyModInfosTsv(readPrettyTsv("modinfos.tsv")).par.groupBy(_.hash).seq
lazy val songlengths = parsePrettySonglengthsTsv(readPrettyTsv("songlengths.tsv")).par.groupBy(_.hash).seq

def md5OfFile(f: File): String = {
  val md = MessageDigest.getInstance("MD5")
  val in = new FileInputStream(f)
  try {
    val buf = new Array[Byte](1 << 16)
    var r = in.read(buf)
    while (r > 0) {
      md.update(buf, 0, r)
      r = in.read(buf)
    }
  } finally in.close()
  md.digest.map("%02x".format(_)).mkString
}

def walk(f: File): Seq[File] =
  if (f.isDirectory) Option(f.listFiles).toSeq.flatten.flatMap(walk)
  else if (f.isFile) Seq(f)
  else Seq.empty

// (source, path, subsong, row without the Source column)
val rows = Buffer[(String, String, Int, Seq[String])]()

def printDir(sourceName: String, dir: File): Unit = {
  val root = dir.toPath.toAbsolutePath
  walk(dir).par.map { f =>
    val md5 = md5OfFile(f)
    (md5, root.relativize(f.toPath.toAbsolutePath).toString, f.length)
  }.seq.sortBy(_._2).foreach { case (md5, path, size) =>
    val hash = md5.take(12)
    if (!(unique && hashOwners._1.containsKey(hash))) {
      val meta = metas.get(hash).map(_.head)
      val modinfo = modinfos.get(hash).map(_.head)
      val format = modinfo.map(_.format).getOrElse("")
      val channels = modinfo.map(m => if (m.channels > 0) m.channels.toString else "").getOrElse("")
      val songinfo = songlengths.get(hash).map(_.head)
      songinfo.map(_.subsongs).filter(_.nonEmpty) match {
        case Some(subsongs) =>
          val minsubsong = songinfo.get.minsubsong
          subsongs.zipWithIndex.foreach { case (ss, i) =>
            rows += ((sourceName, path, minsubsong + i, Seq(hash, path, size.toString, format, "", (minsubsong + i).toString, lenStr(ss.songlength), channels,
              meta.map(_.authors.mkString(" & ")).getOrElse(""),
              meta.map(_.album).getOrElse(""),
              meta.map(_.publishers.mkString(" & ")).getOrElse(""),
              meta.map(m => if (m.year > 0) m.year.toString else "").getOrElse(""),
            )))
          }
        case None =>
          rows += ((sourceName, path, -1, Seq(hash, path, size.toString, format, "", "", "", channels,
            meta.map(_.authors.mkString(" & ")).getOrElse(""),
            meta.map(_.album).getOrElse(""),
            meta.map(_.publishers.mkString(" & ")).getOrElse(""),
            meta.map(m => if (m.year > 0) m.year.toString else "").getOrElse(""),
          )))
      }
    }
  }
}

val headers = Seq(
  "MD5", "Path", "Size", "Format", "Player", "Sub", "Len", "Ch",
  "Authors", "Album", "Publishers", "Year",
)
val maxwidths = Seq(12, 48, 8, 24, 10, 3, 6, 2, 24, 24, 24, 4)
val outHeaders = if (multi) headers.patch(1, Seq("Source"), 0) else headers
val outMaxwidths = if (multi) maxwidths.patch(1, Seq(20), 0) else maxwidths

for (target <- targets) {
  target match {
    case DirTarget(dir) => printDir(targetName(target), dir)

    case SourceTarget(source) =>
      val sourceName = targetName(target)
      val tsvfile = tsvfiles.find(_._2 == source).map(_._1).getOrElse {
        Console.err.println(s"'${source}' has no tsv file (constraints only source)")
        sys.exit(1)
      }
      val entries = parseSourceTsv(tsvfile)
      // short lines (subsongs of the same file) lack the file's other fields; fill from the first subsong
      def fills(v: String, alt: String): String = if (v.isEmpty) alt else v
      def filli(v: Int, alt: Int): Int = if (v <= 0) alt else v
      entries.toSeq.par.flatMap { case (md5, subsongs) =>
        if (unique && !isUnique(md5.take(12), source)) Seq.empty
        else {
        val sorted = subsongs.sortBy(_.subsong)
        val first = sorted.find(_.path.nonEmpty).getOrElse(sorted.head)
        val meta = metas.get(md5.take(12)).map(_.head)
        sorted.map { entry =>
          val filesize = filli(entry.filesize, first.filesize)
          val channels = filli(entry.channels, first.channels)
          (sourceName, fills(entry.path, first.path), entry.subsong, Seq(
            md5.take(12),
            fills(entry.path, first.path),
            if (filesize >= 0) filesize.toString else "",
            fills(entry.format, first.format),
            fills(entry.player, first.player),
            if (entry.subsong >= 0) entry.subsong.toString else "",
            lenStr(entry.songlength),
            if (channels > 0) channels.toString else "",
            meta.map(_.authors.mkString(" & ")).getOrElse(""),
            meta.map(_.album).getOrElse(""),
            meta.map(_.publishers.mkString(" & ")).getOrElse(""),
            meta.map(m => if (m.year > 0) m.year.toString else "").getOrElse(""),
          ))
        }.seq
        }
      }.seq.foreach(r => rows += r)
  }
}

// sorted by Source + Path + Sub
val outRows = rows.sortBy(r => (r._1, r._2, r._3)).map { case (source, _, _, cols) =>
  if (multi) cols.patch(1, Seq(source), 0) else cols
}

if (tsv) {
  println(outHeaders.mkString("\t"))
  outRows.foreach(row => println(row.mkString("\t")))
} else {
  val widths = outHeaders.zip(outMaxwidths).zipWithIndex.map { case ((header, maxWidth), i) =>
    val dataWidth = outRows.map(_(i).length).maxOption.getOrElse(0)
    math.min(maxWidth, math.max(header.length, dataWidth))
  }

  def truncate(text: String, width: Int): String =
    if (text.length <= width) text else text.take(width - 1) + "…"

  // paths show the tail of the path, keeping the filename's end visible
  def truncatePath(text: String, width: Int): String =
    if (text.length <= width) text else "…" + text.takeRight(width - 1)

  def formatRow(values: Seq[String]): String =
    values.zip(outHeaders).zip(widths).map { case ((value, header), width) =>
      val truncated = if (header == "Path") truncatePath(value, width) else truncate(value, width)
      truncated.padTo(width, ' ')
    }.mkString(" | ")

  println()
  println(formatRow(outHeaders))
  println("-" * formatRow(outHeaders).length)
  outRows.foreach(row => println(formatRow(row)))
  println()
}
