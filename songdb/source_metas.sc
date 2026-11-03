#!/usr/bin/env -S scala-cli shebang --suppress-warning-directives-in-multiple-files -q

// SPDX-License-Identifier: GPL-2.0-or-later AND CC-PDM-1.0
// SPDX-AI-Disclosure: ai-assisted
// Copyright (C) 2026 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using jvm 27
//> using scala 3.9
//> using option -opt
//> using option -opt-inline:<sources>
//> using javaOpt -Xmx10G
//> using javaOpt --sun-misc-unsafe-memory-access=allow
//> using javaOpt --enable-native-access=ALL-UNNAMED
//> using javaOpt -XX:+UseCompactObjectHeaders
//> using javaOpt -XX:+UseCompressedOops
//> using javaOpt -XX:MetaspaceSize=256m

// MD5 based metadata and infos from songdb for one (or more) source(s) or directory(s).
// Human readable table to stdout by default, TSV with --tsv.
// --unique-fingerprint, --duplicate-fingerprint and --missing-fingerprint are
// the exceptions: they compare audio fingerprints and need the decompressed
// sources/audio tsvs.

// NOTE: running this first time will fetch/install dependencies etc. which may take a while.

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

//> using file scripts/LockFreeMaps.scala
//> using file scripts/TsvReader.scala
//> using file scripts/chromaprint.sc
//> using file scripts/chromaanalysis.sc
//> using file scripts/chromasimilarity.sc
//> using file scripts/convert.sc
//> using file scripts/dedup.sc
//> using file scripts/md5.sc
//> using file scripts/normalization.sc
//> using file scripts/pretty.sc
//> using file scripts/songlengths.sc
//> using file scripts/tools.sc
//> using file scripts/threadpools.sc
//> using file scripts/sources/audio.sc
//> using file scripts/sources/sources.sc

import java.io.{File, FileInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.security.MessageDigest
import java.util.concurrent.{Callable, ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._

import audio._
import chromaprint._
import chromasimilarity._
import convert._
import pretty._
import sources._
import tools._

// maximum length difference in seconds between compared subsongs in the
// fingerprint modes; mirrors find_dupes.sc MAXLENDIFF
val FP_LEN_WINDOW_SEC = 11.0

val tsv = args.contains("--tsv")
val unique = args.contains("--unique")
val suspicious = args.contains("--suspicious")
val broken = args.contains("--bad")
val duplicate = args.contains("--duplicate")
val uniqueFingerprint = args.contains("--unique-fingerprint")
val duplicateFingerprint = args.contains("--duplicate-fingerprint")
val missingFingerprint = args.contains("--missing-fingerprint")
val positional = args.filterNot(a => a == "--tsv" || a == "--unique" || a == "--suspicious" || a == "--bad" || a == "--duplicate" || a == "--unique-fingerprint" || a == "--duplicate-fingerprint" || a == "--missing-fingerprint")

if (Seq(unique, suspicious, broken, duplicate, uniqueFingerprint, duplicateFingerprint, missingFingerprint).count(identity) > 1) {
  Console.err.println("at most one of --unique, --unique-fingerprint, --duplicate, --duplicate-fingerprint, --missing-fingerprint, --suspicious and --bad can be given")
  sys.exit(1)
}

if (positional.length < 1) {
  Console.err.println("Usage:")
  Console.err.println("  ./source_metas.sc [--tsv] [--suspicious | --bad | --unique[-fingerprint] | --duplicate[-fingerprint] | --missing-fingerprint] <source|all|directory>...")
  Console.err.println()
  Console.err.println("  --tsv                   tab separated output instead of a human readable table")
  Console.err.println("  --suspicious            only suspicious subsongs; ? column: e=error n=nosound t=timeout z=zero length #=bad fingerprint !=duplicate subsong")
  Console.err.println("  --bad                   only md5s with no good subsongs at all")
  Console.err.println("  --unique                unique md5s only found in given sources/directories")
  Console.err.println("  --unique-fingerprint    unique audio fingerprints only found in given sources; directories not supported")
  Console.err.println("  --duplicate             duplicate md5s in given sources/directories")
  Console.err.println("  --duplicate-fingerprint duplicate audio fingerprints in given sources/directories")
  Console.err.println("  --missing-fingerprint   audio fingerprints that are missing from the given sources/directories")
  Console.err.println("  all                     all sources; sources may also be given comma separated")
  Console.err.println()
  Console.err.println("  NOTE: the fingerprint options are heavy")
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

// several sources may be comma separated; an existing directory is taken as-is so
// paths with commas keep working
def splitArg(arg: String): Seq[String] =
  if (new File(arg).isDirectory) Seq(arg)
  else arg.split(',').map(_.trim).filter(_.nonEmpty).toSeq

val targets: Seq[Target] = positional.toSeq.flatMap(splitArg).flatMap { arg =>
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

if (uniqueFingerprint && targets.exists(_.isInstanceOf[DirTarget])) {
  Console.err.println("--unique-fingerprint supports sources only: its rows are source tsv subsongs")
  sys.exit(1)
}

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

// md5s occurring in a source outside the given targets (the given ones count as one group)
lazy val nonTargetHashes: Set[String] = {
  val ts = targets.collect { case SourceTarget(s) => s }.toSet
  sources.tsvs.par.filterNot(t => ts.contains(t._1)).flatMap(_._2.keys.map(_.take(12))).toSet.seq
}

def isUnique(hash: String): Boolean = !nonTargetHashes.contains(hash)

lazy val modinfos = parsePrettyModInfosTsv(readPrettyTsv("modinfos.tsv")).par.groupBy(_.hash).seq
lazy val songlengths = parsePrettySonglengthsTsv(readPrettyTsv("songlengths.tsv")).par.groupBy(_.hash).seq

// the songlengths.tsv subsong for md5 hash + subsong number
def slSubsong(hash: String, subsong: Int): Option[SubsongInfo] =
  songlengths.get(hash).flatMap { infos =>
    val si = infos.head
    val i = subsong - si.minsubsong
    if (i >= 0 && i < si.subsongs.size) Some(si.subsongs(i)) else None
  }

// Bad column: reason char, empty when not bad. Songend counts from either tsv; `#`
// wins over the rest, `!` is the least informative.
val badSongends = Set("error", "nosound", "timeout")

def badReason(sourceSongend: String, sourceSonglength: Int, sl: Option[SubsongInfo]): String =
  if (sl.exists(_.bad)) "#"
  else if (badSongends.contains(sourceSongend)) sourceSongend.take(1)
  else sl.map(_.songend).filter(badSongends.contains).map(_.take(1)).getOrElse {
    if (sourceSonglength <= 0 || sl.exists(_.songlength <= 0)) "z"
    else if (sl.exists(_.duplicate)) "!" else ""
  }

// --bad: every subsong of the md5 is bad; without songlengths data nothing is
// judged, so such md5s are never bad
def allBad(reasons: Iterable[String]): Boolean = reasons.nonEmpty && reasons.forall(_.nonEmpty)

def isBrokenMd5(hash: String, entries: Seq[TsvEntry]): Boolean =
  allBad(entries.map(e => badReason(e.songend, e.songlength, if (e.subsong >= 0) slSubsong(hash, e.subsong) else None)))

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

// target contents cached so the --duplicate scan and the row output share them
lazy val dirFiles: Map[File, Seq[(String, String, Long)]] =
  targets.collect { case DirTarget(d) => d }.map { d =>
    val root = d.toPath.toAbsolutePath
    (d, walk(d).par.map { f =>
      val md5 = md5OfFile(f)
      (md5, root.relativize(f.toPath.toAbsolutePath).toString, f.length)
    }.seq.sortBy(_._2))
  }.toMap

lazy val sourceEntries: Map[Source, Map[String, Buffer[TsvEntry]]] =
  targets.collect { case SourceTarget(s) => s }.flatMap { s =>
    tsvfiles.find(_._2 == s).map(f => s -> parseSourceTsv(f._1))
  }.toMap

// --duplicate: md5s with several paths across all the given sources/directories
lazy val dupMd5s: Set[String] =
  if (!duplicate) Set.empty
  else {
    val paths = scala.collection.mutable.Map[String, scala.collection.mutable.Set[(String, String)]]()
    def add(md5: String, target: String, path: String): Unit =
      paths.getOrElseUpdate(md5, scala.collection.mutable.Set.empty) += ((target, path))
    targets.foreach {
      case t: DirTarget => dirFiles(t.dir).foreach { case (md5, path, _) => add(md5, targetName(t), path) }
      case t: SourceTarget => sourceEntries(t.source).foreach { case (md5, subs) => subs.map(_.path).filter(_.nonEmpty).distinct.foreach(add(md5, targetName(t), _)) }
    }
    paths.filter(_._2.size > 1).keySet.toSet
  }

// The fingerprint modes: see the usage text. The similarity threshold mirrors audio.sc's
// dynamic duplicate threshold on effective (non-silent) seconds. md5s without audio rows
// and bad fingerprints are ignored; directories take part by their md5s.
val fpLenWindowBytes = (FP_LEN_WINDOW_SEC * persecondbytes).toInt
val fpThreads = Runtime.getRuntime.availableProcessors

final case class CorpusRow(md5: String, subsong: Int, audioBytes: Int, audioMd5: String, key: Long, effBytes: Int, fp: FP)

lazy val fpShards: Array[java.io.File] = audioShardFiles()

// owned by the caller and shut down there (unlike the shared build pools)
def fpPool(): ExecutorService =
  Executors.newFixedThreadPool(fpThreads, { (r: Runnable) =>
    val t = new Thread(r, "fp")
    t.setDaemon(true)
    t
  })

// progress to stderr, as in find_dupes/audio_match. One line per run: nested phases
// share the outer line (depth-guarded); total 0 = phases of unknown size
val fpDone = AtomicInteger(0)
val fpDepth = AtomicInteger(0)

def fpProgressStart(total: Int): Unit =
  if (fpDepth.getAndIncrement() == 0) {
    fpDone.set(0)
    System.err.print(if (total > 0) s"Processing (x/$total) " else "Processing ")
  }

def fpProgressStep(): Unit = System.err.print(s".${fpDone.incrementAndGet()}.")

def fpProgressDone(): Unit =
  if (fpDepth.decrementAndGet() == 0) System.err.print(" done.\n")

// shuffled small chunks: unmatched queries scan their whole window, so coarse splitting
// would leave one thread running long after the others idle
def fpMapChunks[A, B](pool: ExecutorService, rows: Seq[A], chunks: Int = 0, progress: Boolean = false)(f: A => Iterable[B]): Set[B] =
  val chunkSize = if (chunks > 0) math.max(1, (rows.length + chunks - 1) / chunks) else math.max(16, rows.length / (fpThreads * 8))
  val groups = scala.util.Random.shuffle(rows.toIndexedSeq).grouped(chunkSize).toSeq
  if (progress) fpProgressStart(groups.length)
  val hits = groups.map { grp =>
    pool.submit(new Callable[Set[B]] {
      def call(): Set[B] =
        val hs = grp.iterator.flatMap(f).toSet
        if (progress) fpProgressStep()
        hs
    })
  }.flatMap(_.get()).toSet
  if (progress) fpProgressDone()
  hits

def fpChunks[A](pool: ExecutorService, rows: Seq[A], chunks: Int = 0, progress: Boolean = false)(f: A => Boolean): Set[A] =
  fpMapChunks(pool, rows, chunks, progress)(a => if (f(a)) Set(a) else Set.empty)

// first index with bytes(i) >= key. Arrays.binarySearch returns an arbitrary index
// among equal values and would skip rows equal to the bound
def lowerBound(bytes: Array[Int], key: Int): Int =
  var lo = 0
  var hi = bytes.length
  while (lo < hi) {
    val mid = (lo + hi) >>> 1
    if (bytes(mid) < key) lo = mid + 1 else hi = mid
  }
  lo

// a bad corpus fingerprint is a duplicate of nothing (audio.sc), checked only on pairs
// that pass the threshold. skipMd5s are group-exclusive md5s: a match within the group
// keeps the query unique, a match to an md5 also occurring outside it leaks it
def hasDupIn(q: CorpusRow, rows: Array[CorpusRow], bytes: Array[Int], skipMd5s: Set[String]): Boolean = {
  val qSec = q.effBytes.toDouble / persecondbytes
  var lo = lowerBound(bytes, q.audioBytes - fpLenWindowBytes)
  val hiLimit = q.audioBytes + fpLenWindowBytes
  var i = lo
  var dup = false
  while (i < rows.length && !dup && bytes(i) <= hiLimit) {
    val r = rows(i)
    if (r.md5 != q.md5 && !skipMd5s.contains(r.md5)) {
      if ((r.audioMd5.nonEmpty && r.audioMd5 == q.audioMd5) || (r.key != 0L && r.key == q.key)) dup = !r.fp.isBad
      else {
        val rSec = r.effBytes.toDouble / persecondbytes
        if (chromaSimilarityFPs(q.fp, r.fp) >= fpThreshold(qSec, rSec)) dup = !r.fp.isBad
      }
    }
    i += 1
  }
  dup
}

// md5s of the given targets; directories contribute the md5s of their files
// (only those found in the audio corpus ever become query rows)
lazy val fpQueryMd5s: Set[String] =
  (targets.collect { case SourceTarget(s) => sourceEntries(s).keys.map(_.take(12)).toSet } ++
    targets.collect { case DirTarget(d) => dirFiles(d).map(e => e._1.take(12)).toSet }).reduce(_ ++ _)

// fingerprinted audio rows of the given targets — the queries for the fingerprint modes
lazy val fpTargetRows: Array[CorpusRow] = {
  val queryMd5s = fpQueryMd5s
  val pool = fpPool()
  try {
    // one task per shard, .par would not split a 16 element collection at all
    fpShards.map { f =>
      pool.submit(new Callable[Seq[CorpusRow]] {
        def call(): Seq[CorpusRow] = parseAudioTsv(f.getAbsolutePath, withSimHash = false, md5s = queryMd5s)
          .withFilter(_.audioChromaprint.nonEmpty).map { af =>
            // the global fpCache shares each FP across sources and with fpCorpus;
            // large target sets would otherwise OOM on copies
            val (key, fp) = cacheChromaprint(af.audioChromaprint)
            // raw subsong: the audio tsv numbering matches the source tsvs as-is
            CorpusRow(af.md5, af.subsong, af.audioBytes, af.audioMd5, key, fp.effectiveBytes(af.audioBytes), fp)
          }.toSeq
      })
    }.flatMap(_.get()).toArray
  } finally pool.shutdown()
}

// the whole fingerprinted corpus, one task per shard; kept resident so the per-query
// comparisons run fully parallel
lazy val fpCorpus: Array[Array[CorpusRow]] = {
  val pool = fpPool()
  try {
    fpProgressStart(fpShards.length)
    fpShards.map { f =>
      pool.submit(new Callable[Array[CorpusRow]] {
        def call(): Array[CorpusRow] =
          parseAudioTsv(f.getAbsolutePath, withSimHash = false)
            .withFilter(_.audioChromaprint.nonEmpty).map { af =>
              val (key, fp) = cacheChromaprint(af.audioChromaprint)
              CorpusRow(af.md5, af.subsong, af.audioBytes, af.audioMd5, key, fp.effectiveBytes(af.audioBytes), fp)
            }.toArray.sortBy(_.audioBytes)
      })
    }.map { t => val r = t.get(); fpProgressStep(); r }
  }
  finally { fpProgressDone(); pool.shutdown() }
}

lazy val fpCorpusRows: Array[CorpusRow] = fpCorpus.flatten
lazy val fpCorpusMd5s: Set[String] = fpCorpusRows.iterator.map(_.md5).toSet

// a (md5, subsong) is unique only when none of its audio rows matched
lazy val fpUniqueSubsongs: Set[(String, Int)] = {
  val groupExclusive = fpQueryMd5s -- nonTargetHashes
  val all = fpTargetRows.filter(r => !nonTargetHashes.contains(r.md5) && !r.fp.isBad)
  val pairs = all.map(r => (r.md5, r.subsong)).toSet
  val pool = fpPool()
  try {
    // direct leaks: a match to a corpus row whose md5 occurs outside the group.
    // Leaked rows need no judging in the remaining shards
    var live = all
    val leaked = scala.collection.mutable.Set[(String, Int)]()
    fpProgressStart(fpCorpus.length)
    for ((rows, s) <- fpCorpus.zipWithIndex if live.nonEmpty) {
      val bytes = rows.map(_.audioBytes)
      val shardLeaked = fpChunks(pool, live.toSeq)(hasDupIn(_, rows, bytes, groupExclusive))
        .map(r => (r.md5, r.subsong))
      leaked ++= shardLeaked
      live = live.filterNot(r => shardLeaked.contains((r.md5, r.subsong)))
      fpProgressStep()
    }
    fpProgressDone()
    // transitive leaks: a match to a row that itself leaked. Only target rows can be
    // intermediaries. Each round scans the (small) unique set for a leaked partner,
    // repeating until stable
    val trows = all.sortBy(_.audioBytes)
    val tbytes = trows.map(_.audioBytes)
    var uniqueRows = all.filterNot(r => leaked.contains((r.md5, r.subsong)))
    var found = true
    while (found && uniqueRows.nonEmpty) {
      val newlyLeaked = fpChunks(pool, uniqueRows.toSeq, chunks = fpThreads) {
        matchesLeaked(_, trows, tbytes, leaked.toSet)
      }.map(r => (r.md5, r.subsong))
      found = newlyLeaked.nonEmpty
      leaked ++= newlyLeaked
      uniqueRows = uniqueRows.filterNot(r => newlyLeaked.contains((r.md5, r.subsong)))
    }
    pairs -- leaked
  } finally pool.shutdown()
}

// does q match a leaked target row (other than itself)
def matchesLeaked(q: CorpusRow, rows: Array[CorpusRow], bytes: Array[Int], leaked: Set[(String, Int)]): Boolean = {
  val qSec = q.effBytes.toDouble / persecondbytes
  var i = lowerBound(bytes, q.audioBytes - fpLenWindowBytes)
  val hiLimit = q.audioBytes + fpLenWindowBytes
  var hit = false
  while (i < rows.length && !hit && bytes(i) <= hiLimit) {
    val r = rows(i)
    if ((r.md5 != q.md5 || r.subsong != q.subsong) && leaked.contains((r.md5, r.subsong)) && !r.fp.isBad) {
      val rSec = r.effBytes.toDouble / persecondbytes
      if ((r.audioMd5.nonEmpty && r.audioMd5 == q.audioMd5) || (r.key != 0L && r.key == q.key) || chromaSimilarityFPs(q.fp, r.fp) >= fpThreshold(qSec, rSec)) hit = true
    }
    i += 1
  }
  hit
}

// every matching (md5, subsong) of q under a different row. crossMd5 restricts the
// partners to other md5s
def dupPartnerKeys(q: CorpusRow, rows: Array[CorpusRow], bytes: Array[Int], crossMd5: Boolean = false): scala.collection.Set[(String, Int)] = {
  val qSec = q.effBytes.toDouble / persecondbytes
  var lo = lowerBound(bytes, q.audioBytes - fpLenWindowBytes)
  val hiLimit = q.audioBytes + fpLenWindowBytes
  val out = scala.collection.mutable.Set[(String, Int)]()
  var i = lo
  while (i < rows.length && bytes(i) <= hiLimit) {
    val r = rows(i)
    if ((r.md5 != q.md5 || r.subsong != q.subsong) && (!crossMd5 || r.md5 != q.md5) && !r.fp.isBad) {
      val rSec = r.effBytes.toDouble / persecondbytes
      if ((r.audioMd5.nonEmpty && r.audioMd5 == q.audioMd5) || (r.key != 0L && r.key == q.key) || chromaSimilarityFPs(q.fp, r.fp) >= fpThreshold(qSec, rSec))
        out += ((r.md5, r.subsong))
    }
    i += 1
  }
  out
}

// connected components of an undirected graph: node -> 1-based group number,
// the groups ordered by their smallest node. Matching is not transitive, so
// every edge is collected, not just each row's first partner.
def components[T](nodes: Set[T], edges: Iterable[(T, T)])(using Ordering[T]): Map[T, Int] =
  val parent = scala.collection.mutable.Map[T, T]()
  def find(x: T): T =
    if (parent.getOrElseUpdate(x, x) != x) { val r = find(parent(x)); parent(x) = r; r } else x
  edges.foreach { case (a, b) =>
    val ra = find(a)
    val rb = find(b)
    if (ra != rb) parent(ra) = rb
  }
  nodes.groupBy(find).toSeq.sortBy(_._2.min).zipWithIndex
    .flatMap { case ((_, g), gi) => g.map(_ -> (gi + 1)) }.toMap

// md5 -> coarse group: md5s projected from the same fine component share a group
def coarseFromFine(fine: Map[(String, Int), Int], md5s: Set[String]): Map[String, Int] =
  components(
    md5s,
    fine.groupBy(_._2).valuesIterator.flatMap(g =>
      g.keysIterator.map(_._1).toSeq.distinct.combinations(2).map(c => (c(0), c(1)))).toSeq,
  )

// (md5, subsong) -> fine match group (the separator), md5 -> coarse group (the order).
// Unique rows match nothing outside the given sources, so only within-group matches
// are shared; rows with no partner are singleton fine groups.
lazy val fpUniqueData: (Map[String, Int], Map[(String, Int), Int]) = {
  val rows = fpTargetRows.filter(r => fpUniqueSubsongs.contains((r.md5, r.subsong))).sortBy(_.audioBytes)
  val bytes = rows.map(_.audioBytes)
  val pool = fpPool()
  try {
    val edges = fpMapChunks(pool, rows.toSeq, chunks = fpThreads) { q =>
      dupPartnerKeys(q, rows, bytes).map(((q.md5, q.subsong), _))
    }
    val fine = components(rows.iterator.map(r => (r.md5, r.subsong)).toSet, edges)
    (coarseFromFine(fine, rows.iterator.map(_.md5).toSet), fine)
  } finally pool.shutdown()
}
lazy val fpUniqueGroups: Map[(String, Int), Int] = fpUniqueData._2
lazy val fpUniqueMd5Groups: Map[String, Int] = fpUniqueData._1

// every matching md5 of q under a different md5; unlike hasDupIn no
// short-circuit: the partners build the --missing-fingerprint groups
def dupPartners(q: CorpusRow, rows: Array[CorpusRow], bytes: Array[Int]): scala.collection.Set[String] = {
  val qSec = q.effBytes.toDouble / persecondbytes
  var lo = lowerBound(bytes, q.audioBytes - fpLenWindowBytes)
  val hiLimit = q.audioBytes + fpLenWindowBytes
  val out = scala.collection.mutable.Set[String]()
  var i = lo
  while (i < rows.length && bytes(i) <= hiLimit) {
    val r = rows(i)
    if (r.md5 != q.md5) {
      val rSec = r.effBytes.toDouble / persecondbytes
      if ((r.audioMd5.nonEmpty && r.audioMd5 == q.audioMd5) || (r.key != 0L && r.key == q.key) || chromaSimilarityFPs(q.fp, r.fp) >= fpThreshold(qSec, rSec))
        if (!r.fp.isBad) out += r.md5
    }
    i += 1
  }
  out
}

// Coarse and fine grouping over the same edges: the coarse md5 group keeps related md5s
// adjacent, the fine (md5, subsong) group separates within it. A coarse group always
// holds >= 2 md5s.
lazy val fpDupData: (Map[String, Int], Map[(String, Int), Int]) = {
  val rows = fpTargetRows.sortBy(_.audioBytes)
  val bytes = rows.map(_.audioBytes)
  val queries = rows.filterNot(_.fp.isBad)
  val pool = fpPool()
  try {
    val edges = fpMapChunks(pool, queries.toSeq, chunks = fpThreads, progress = true) { q =>
      dupPartnerKeys(q, rows, bytes, crossMd5 = true).map(((q.md5, q.subsong), _))
    }
    val keys = edges.iterator.flatMap { case (a, b) => Iterator(a, b) }.toSet
    (components(keys.iterator.map(_._1).toSet, edges.map { case ((a, _), (b, _)) => (a, b) }), components(keys, edges))
  } finally pool.shutdown()
}
lazy val fpDupGroups: Map[String, Int] = fpDupData._1
// the matching subsongs with their fine match group
lazy val fpDupMatchGroups: Map[(String, Int), Int] = fpDupData._2
lazy val fpDupMatchSubsongs: Set[(String, Int)] = fpDupMatchGroups.keySet

// corpus md5s absent from the given targets with no fingerprint near-duplicate among
// them — the inverse of --unique-fingerprint
lazy val fpMissingMd5s: Set[String] = {
  val targetMd5s = fpQueryMd5s
  // target rows are corpus rows: take them from the resident corpus, a second
  // materialization OOMs for broad source sets
  val trows = fpCorpusRows.filter(r => targetMd5s.contains(r.md5)).sortBy(_.audioBytes)
  val tbytes = trows.map(_.audioBytes)
  val pool = fpPool()
  try {
    // an md5 is missing only when none of its rows matched
    val res = fpMapChunks(pool, fpCorpusRows.toSeq, chunks = fpThreads * 8, progress = true) { c =>
      if (!targetMd5s.contains(c.md5) && !c.fp.isBad) Set((c.md5, hasDupIn(c, trows, tbytes, Set.empty)))
      else Set.empty
    }
    res.map(_._1).toSet -- res.collect { case (m, true) => m }.toSet
  } finally pool.shutdown()
}

// Like fpDupData, over the missing subset only
lazy val fpMissingData: (Map[String, Int], Map[(String, Int), Int]) = {
  val rows = fpCorpusRows.filter(r => fpMissingMd5s.contains(r.md5)).sortBy(_.audioBytes)
  val bytes = rows.map(_.audioBytes)
  val queries = rows.filterNot(_.fp.isBad)
  val pool = fpPool()
  try {
    val edges = fpMapChunks(pool, queries.toSeq, chunks = fpThreads, progress = true) { q =>
      dupPartnerKeys(q, rows, bytes, crossMd5 = true).map(((q.md5, q.subsong), _))
    }
    val fine = components(rows.iterator.map(r => (r.md5, r.subsong)).toSet, edges)
    (components(fpMissingMd5s, edges.map { case ((a, _), (b, _)) => (a, b) }), fine)
  } finally pool.shutdown()
}
lazy val fpMissingMd5Groups: Map[String, Int] = fpMissingData._1
lazy val fpMissingGroups: Map[(String, Int), Int] = fpMissingData._2
// missing subsongs without an audio row: one separator per md5, negative to keep them
// apart from the fine groups
lazy val fpMissingNoFp: Map[String, Int] =
  fpMissingMd5s.toSeq.sorted.zipWithIndex.map { case (m, i) => m -> -(i + 1) }.toMap

def printDir(sourceName: String, dir: File): Unit = {
  dirFiles(dir).foreach { case (md5, path, size) =>
    val hash = md5.take(12)
    if (!(unique && nonTargetHashes.contains(hash)) && !(duplicate && !dupMd5s.contains(md5)) && !(duplicateFingerprint && !fpDupGroups.contains(hash))) {
      val meta = metas.get(hash).map(_.head)
      val modinfo = modinfos.get(hash).map(_.head)
      val format = modinfo.map(_.format).getOrElse("")
      val channels = modinfo.map(m => if (m.channels > 0) m.channels.toString else "").getOrElse("")
      val songinfo = songlengths.get(hash).map(_.head)
      songinfo.map(_.subsongs).filter(_.nonEmpty) match {
        case Some(subsongs) if !broken || allBad(subsongs.map(ss => badReason(ss.songend, ss.songlength, Some(ss)))) =>
          val minsubsong = songinfo.get.minsubsong
          val collapsed = unique || duplicate
          val many = subsongs.size > 1
          subsongs.zipWithIndex.foreach { case (ss, i) =>
            if (!(collapsed && i > 0) && (!duplicateFingerprint || fpDupMatchSubsongs.contains((hash, minsubsong + i)))) {
            val reason = badReason(ss.songend, ss.songlength, Some(ss))
            if (!suspicious || reason.nonEmpty) {
            rows += ((sourceName, path, minsubsong + i, Seq(hash, path, size.toString, format, "", (if (collapsed && many) "*" else (minsubsong + i).toString), if (collapsed && many) "*" else lenStr(ss.songlength), channels,
              meta.map(_.authors.mkString(" & ")).getOrElse(""),
              meta.map(_.album).getOrElse(""),
              meta.map(_.publishers.mkString(" & ")).getOrElse(""),
              meta.map(m => if (m.year > 0) m.year.toString else "").getOrElse(""),
              if (collapsed && many) "*" else reason,
            )))
            }
            }
          }
        case None =>
          // no songlengths entry: nothing to judge, never bad
          val reason = ""
          if (!broken && (!suspicious || reason.nonEmpty)) {
          rows += ((sourceName, path, -1, Seq(hash, path, size.toString, format, "", "", "", channels,
            meta.map(_.authors.mkString(" & ")).getOrElse(""),
            meta.map(_.album).getOrElse(""),
            meta.map(_.publishers.mkString(" & ")).getOrElse(""),
            meta.map(m => if (m.year > 0) m.year.toString else "").getOrElse(""),
            reason,
          )))
          }
        // --bad: at least one good subsong, or none judged -> not bad
        case _ =>
      }
    }
  }
}

// the output columns with their max widths; the Source column is always shown:
// --missing-fingerprint rows come from the sources having the md5s, and even a
// single target's rows are clearer with it
val outColumns = Seq(
  Col("MD5", 12), Col("Source", 20), Col("Path", 46, middle = true), Col("Size", 8), Col("Format", 23),
  Col("Player", 10), Col("Sub", 3), Col("Len", 6), Col("Ch", 2), Col("Authors", 23), Col("Album", 23),
  Col("Publishers", 23), Col("Year", 4), Col("?", 1),
)

// short lines (subsongs of the same file) lack the file's other fields; fill from the first subsong
def fills(v: String, alt: String): String = if (v.isEmpty) alt else v
def filli(v: Int, alt: Int): Int = if (v <= 0) alt else v

// --unique/--duplicate: one row per path, Sub/Len/? collapsed to '*' for multi-subsong
// paths; the fingerprint modes print the matching subsongs with their real numbers
def emitSourceRows(sourceName: String, entries: Map[String, Buffer[TsvEntry]]): Unit =
  entries.toSeq.par.flatMap { case (md5, subsongs) =>
    if (unique && !isUnique(md5.take(12))) Seq.empty
    else if (duplicate && !dupMd5s.contains(md5)) Seq.empty
    else if (broken && !isBrokenMd5(md5.take(12), subsongs.toSeq)) Seq.empty
    else if (missingFingerprint && !fpMissingMd5s.contains(md5.take(12))) Seq.empty
    else if (duplicateFingerprint && !fpDupGroups.contains(md5.take(12))) Seq.empty
    else {
      val sorted = subsongs.sortBy(_.subsong)
      val first = sorted.find(_.path.nonEmpty).getOrElse(sorted.head)
      val meta = metas.get(md5.take(12)).map(_.head)
      // the subsongs of one path: the continuation lines lack the path
      val groups = Buffer[Buffer[TsvEntry]]()
      sorted.foreach { e =>
        if (e.path.nonEmpty || groups.isEmpty) groups += Buffer(e) else groups.last += e
      }
      val collapsed = unique || duplicate
      val paths = groups.filter(_.head.path.nonEmpty)
      val selected: Seq[(TsvEntry, Int)] =
        if (collapsed) (if (paths.nonEmpty) paths else Seq(sorted.toSeq)).map(g => (g.head, g.size)).toSeq
        else sorted.map(e => (e, 1)).toSeq
      selected.flatMap { case (entry, count) =>
        val filesize = filli(entry.filesize, first.filesize)
        val channels = filli(entry.channels, first.channels)
        val sl = if (entry.subsong >= 0) slSubsong(md5.take(12), entry.subsong) else None
        val reason = badReason(entry.songend, entry.songlength, sl)
        if ((suspicious && reason.isEmpty) || (uniqueFingerprint && !fpUniqueSubsongs.contains((md5.take(12), entry.subsong))) || (duplicateFingerprint && !fpDupMatchSubsongs.contains((md5.take(12), entry.subsong)))) None
        else Some((sourceName, fills(entry.path, first.path), entry.subsong, Seq(
          md5.take(12),
          fills(entry.path, first.path),
          if (filesize >= 0) filesize.toString else "",
          fills(entry.format, first.format),
          fills(entry.player, first.player),
          if (collapsed && count > 1) "*" else if (entry.subsong >= 0) entry.subsong.toString else "",
          if (collapsed && count > 1) "*" else lenStr(entry.songlength),
          if (channels > 0) channels.toString else "",
          meta.map(_.authors.mkString(" & ")).getOrElse(""),
          meta.map(_.album).getOrElse(""),
          meta.map(_.publishers.mkString(" & ")).getOrElse(""),
          meta.map(m => if (m.year > 0) m.year.toString else "").getOrElse(""),
          if (collapsed && count > 1) "*" else reason,
        )))
      }
    }
  }.seq.foreach(r => rows += r)

// --missing-fingerprint reports from every source having the md5s, the other modes
// only from the given targets. Sequential: the rows Buffer is shared and
// emitSourceRows parallelizes internally.
if (missingFingerprint) {
  fpProgressStart(fpShards.length + fpThreads * 8 + fpThreads + sources.tsvs.length)
  val targetFp = fpQueryMd5s.intersect(fpCorpusMd5s)
  val outside = fpCorpusMd5s -- fpQueryMd5s
  // force the comparison and grouping phases inside the progress line
  fpMissingMd5s.size
  fpMissingGroups.size
  sources.tsvs.foreach { case (source, entries) =>
    emitSourceRows(source.toString, entries)
    fpProgressStep()
  }
  fpProgressDone()
}
else {
  if (uniqueFingerprint) fpProgressStart(fpShards.length * 2)
  else if (duplicateFingerprint) fpProgressStart(fpThreads)
  for (target <- targets) {
    target match {
      case DirTarget(dir) => printDir(targetName(target), dir)

      case SourceTarget(source) => emitSourceRows(targetName(target), sourceEntries(source))
    }
  }
  if (uniqueFingerprint || duplicateFingerprint) fpProgressDone()
}

// target order + Path + Sub; md5 blocks sort where their first target has them. The
// fingerprint modes sort by coarse md5 group then fine match group and separate the
// match groups; --unique/--duplicate keep the md5 grouping but need no separator.
// The first value below is the separator group (fine group / empty)
val separated = duplicateFingerprint || missingFingerprint || uniqueFingerprint
val targetNames = targets.map(targetName)
val targetOrder = (targetNames ++ Source.values.map(_.toString).distinct
  .filterNot(targetNames.toSet).sorted).zipWithIndex.toMap
def targetRank(source: String): Int = targetOrder.getOrElse(source, targetNames.length)
def rowKey(r: (String, String, Int, Seq[String])) = (targetRank(r._1), r._2, r._3)
val md5First = rows.groupBy(_._4.head).view.mapValues(_.map(rowKey).min).toMap
def fpGroup(r: (String, String, Int, Seq[String])) =
  if (duplicateFingerprint) fpDupMatchGroups((r._4.head, r._3))
  else if (uniqueFingerprint) fpUniqueGroups((r._4.head, r._3))
  else fpMissingGroups.getOrElse((r._4.head, r._3), fpMissingNoFp(r._4.head))
def fpCoarse(r: (String, String, Int, Seq[String])) =
  if (duplicateFingerprint) fpDupGroups(r._4.head)
  else if (uniqueFingerprint) fpUniqueMd5Groups(r._4.head)
  else fpMissingMd5Groups(r._4.head)
val coarseFirst =
  if (duplicateFingerprint || uniqueFingerprint || missingFingerprint)
    rows.groupBy(fpCoarse).view.mapValues(_.map(rowKey).min).toMap
  else Map.empty[Int, (Int, String, Int)]
val fineFirst =
  if (duplicateFingerprint || uniqueFingerprint || missingFingerprint)
    rows.groupBy(fpGroup).view.mapValues(_.map(rowKey).min).toMap
  else Map.empty[Int, (Int, String, Int)]
val outRows = (if (duplicateFingerprint || uniqueFingerprint || missingFingerprint)
    rows.sortBy(r =>
      (coarseFirst(fpCoarse(r)), fpCoarse(r), fineFirst(fpGroup(r)), fpGroup(r), md5First(r._4.head), r._4.head, rowKey(r)))
  else rows.sortBy { r =>
    if (separated || unique || duplicate) (md5First(r._4.head), r._4.head, rowKey(r))
    else (rowKey(r), "", rowKey(r))
  }
).map { case (source, _, subsong, cols) =>
  val group =
    if (duplicateFingerprint || uniqueFingerprint || missingFingerprint) fpGroup((source, "", subsong, cols)).toString
    else if (separated) cols.head
    else ""
  (group, cols.patch(1, Seq(source), 0))
}

if (tsv) {
  println(outColumns.map(_.header).mkString("\t"))
  var group = ""
  outRows.foreach { case (g, row) =>
    if (separated && group.nonEmpty && g != group) println()
    group = g
    println(row.mkString("\t"))
  }
} else {
  val table = new Table(outColumns, outRows.iterator.map(_._2).toList)
  // the group separator rule, as in find_dupes between the lookup and the matches
  val separators = (1 until outRows.length)
    .filter(i => separated && outRows(i - 1)._1.nonEmpty && outRows(i)._1 != outRows(i - 1)._1)
    .toSet
  table.print(separators)
}
