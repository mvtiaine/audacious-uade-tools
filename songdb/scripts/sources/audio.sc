// SPDX-License-Identifier: GPL-2.0-or-later AND CC-PDM-1.0
// SPDX-AI-Disclosure: ai-assisted
// Copyright (C) 2025-2026 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import java.nio.file.Paths
import java.util.Arrays
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.Callable
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ExecutionContextTaskSupport

import chromaprint._
import songlengths._

val persecondbytes = 2 * 11025

final case class AudioFingerprint (
  md5: String,
  player: String,
  subsong: Int,
  normalizedSubsong: Int, // normalized to start from 1
  audioBytes: Int,
  audioMd5: String,
  // HEX form of the xxhash64 chromaprint key; withSimHash=false this holds the base64 chromaprint itself.
  audioChromaprint: String,
  // The same key as `audioChromaprint` as a primitive Long (0 = absent).
  audioChromaprintKey: Long,
  audioHash: String,
  audioTag: String,
  // The interned FP behind `audioChromaprintKey` (null when there is none). SHARED instance.
  fp: FP = null
) {
  // Explicit hashCode/equals EXCLUDING `fp` and audioChromaPrint (audioChromaPrintKey is sufficient)
  override def hashCode: Int = {
    var h = md5.hashCode
    h = h * 31 + player.hashCode
    h = h * 31 + subsong
    h = h * 31 + normalizedSubsong
    h = h * 31 + audioBytes
    h = h * 31 + audioMd5.hashCode
    h = h * 31 + audioChromaprintKey.hashCode
    h = h * 31 + audioHash.hashCode
    h = h * 31 + audioTag.hashCode
    h
  }

  override def equals(obj: Any): Boolean = obj match {
    case o: AudioFingerprint =>
      // `fp` and `audioChromaprint` are deliberately absent
      md5 == o.md5 && player == o.player && subsong == o.subsong &&
      normalizedSubsong == o.normalizedSubsong && audioBytes == o.audioBytes &&
      audioMd5 == o.audioMd5 && audioChromaprintKey == o.audioChromaprintKey &&
      audioHash == o.audioHash && audioTag == o.audioTag
    case _ => false
  }

  lazy val effectiveAudioBytes: Int = {
    if (audioChromaprintKey == 0L) audioBytes
    else {
      if (fp == null) throw new IllegalStateException("effectiveAudioBytes requires a cached fingerprint (withSimHash=true)")
      if (fp.length == 0) audioBytes
      else {
        val (s, e) = fp.contentBounds
        (audioBytes.toLong * (e - s) / fp.length).toInt
      }
    }
  }
}

final class SubsongTags(
  val valid: Array[(mutable.Seq[String], mutable.Seq[AudioFingerprint])],
  val tags: Array[Array[String]],
  val bytes: Array[Int],
  val subs: Array[Array[Int]],
  val index: Array[Long]
)

val audioTsvSizes = Map(
  "sources/audio/audio_0.tsv" -> 162244219L,
  "sources/audio/audio_1.tsv" -> 164567239L,
  "sources/audio/audio_2.tsv" -> 163394653L,
  "sources/audio/audio_3.tsv" -> 163523252L,
  "sources/audio/audio_4.tsv" -> 163229718L,
  "sources/audio/audio_5.tsv" -> 162149062L,
  "sources/audio/audio_6.tsv" -> 162768888L,
  "sources/audio/audio_7.tsv" -> 164877542L,
  "sources/audio/audio_8.tsv" -> 162623874L,
  "sources/audio/audio_9.tsv" -> 163775618L,
  "sources/audio/audio_a.tsv" -> 160023725L,
  "sources/audio/audio_b.tsv" -> 164023188L,
  "sources/audio/audio_c.tsv" -> 164255398L,
  "sources/audio/audio_d.tsv" -> 162465611L,
  "sources/audio/audio_e.tsv" -> 162917749L,
  "sources/audio/audio_f.tsv" -> 164257309L
)

def parseAudioTsv(tsv: String, withSimHash: Boolean, md5s: Set[String] = Set.empty, lengths: Set[Int] = Set.empty) = {
  var prevMd5 = ""
  var prevPlayer = ""
  var fixsubsong = false
  
  val f = Paths.get(tsv).toFile
  val key = tsv.split("/").takeRight(3).mkString("/")
  if (f.length() != audioTsvSizes(key)) {
    System.err.println()
    System.err.println()
    System.err.println(s"ERROR: audio TSV file ${tsv} has unexpected size ${f.length()} (expected ${audioTsvSizes(key)})")
    System.err.println()
    System.err.println(s"Make sure the audio TSV files are decompressed correctly from the zstd archives in 'sources/audio' (e.g. zstd -d sources/audio/audio_*.zst)")
    System.err.println(s"And that you are using the latest version of the files. The source code and the audio TSV files must be in sync.")
    System.err.println(s"See README.md for instructions.")
    System.exit(1)
  }

  val out = Buffer[AudioFingerprint]()

  TsvReader.foreach(f) { line =>
    val md5 = line.fieldPrefix(0, 12)
    val player = line.field(1)
    val subsong = line.int(2)
    val audioBytes = line.int(3)
    var normalizedSubsong = subsong
    if (md5 != prevMd5 || player != prevPlayer) {
      prevMd5 = md5
      prevPlayer = player
      fixsubsong = false
    }
    if (subsong == 0) {
      fixsubsong = true
    }
    if (fixsubsong) {
      normalizedSubsong += 1
    }
    if (audioBytes > 0 && (md5s.isEmpty || md5s.contains(md5)) && (lengths.isEmpty || lengths.exists(len => Math.abs(audioBytes.toDouble / persecondbytes - len.toDouble / persecondbytes) <= 6.66))) {
      val audioMd5 = line.fieldPrefixOrElse(4, 12, "")
      val audioChromaprint = line.fieldOpt(5)
      val (audioChromaprintKey, audioChromaprintFP) =
        if (withSimHash && audioChromaprint.nonEmpty) cacheChromaprint(audioChromaprint) else (0L, null)
      val audioChromaprintHash = if (audioChromaprintKey != 0L) audioChromaprintKey.toHexString else audioChromaprint
      // require at least 9s of audio for simhash comparison to minimize false positives
      val audioSimHash = if (withSimHash && audioChromaprint.nonEmpty && audioBytes > persecondbytes * 9) {
        val fp = audioChromaprintFP
        val numHashes = Math.max(1, audioBytes / (persecondbytes * 3)) // one hash per 3s of audio
        SimHash(fp.data, numHashes).toString(16)
      } else ""
      val audioHash = Seq(audioSimHash, audioChromaprintHash, audioMd5).filter(_.nonEmpty).head
      val audioTags =
        if (withSimHash) {
          val lBuckets =
            if (audioBytes > persecondbytes * 9) Seq(((audioBytes + persecondbytes*3) / (persecondbytes*6)), ((audioBytes - persecondbytes*3) / (persecondbytes*6))).distinct
            else Seq.empty[Int]
          val sl = songlengths.songlengthsByMd5(md5)
          val format = if (withSimHash) sl.head.format else ""
          val atari =
            format.endsWith("ST") ||
            format.contains(" ST ") ||
            format.contains("YM2149") ||
            format.contains(" PSG") ||
            format.contains("POKEYNoise")
          val prefix =
            if (atari) "atari"
            else player
          if (audioHash == audioSimHash)
            for { l <- lBuckets }
              yield prefix + "-l-" + l
          else Seq(prefix + "-h-" + audioHash)
        } else Seq("")
      audioTags.sorted.map(audioTag => out += AudioFingerprint(
        md5,
        player,
        subsong,
        normalizedSubsong,
        audioBytes,
        audioMd5,
        audioChromaprintHash,
        audioChromaprintKey,
        audioHash,
        audioTag,
        audioChromaprintFP,
      ))
    }
  }

  out.distinct
}

lazy val (
  audioHashesByMd5,
  components,
  duplicatesForTag,
  duplicateSubsongsByPlayerAndMd5
) = {
  val parseTasks = Paths.get("sources/audio").toFile.listFiles
    .filter(_.getName.endsWith(".tsv"))
    .map(tsv => threadpools.workerPool.submit[Buffer[AudioFingerprint]](
      () => parseAudioTsv(tsv.getAbsolutePath, withSimHash = true)))
  val parsedAudioFingerprints = parseTasks.map(_.get()).reduce(_ ++ _)
  val parsedPar = {
    val p = parsedAudioFingerprints.par
    p.tasksupport = new ExecutionContextTaskSupport(threadpools.workerEc)
    p
  }
  val filteredAudioFingerprints =
    parsedPar.groupBy(_.audioTag).filter(_._1.nonEmpty).values.flatMap { fps =>
      if (fps.size == 1 && songlengths.songlengthsByMd5(fps.head.md5).forall(_.subsongs.size == 1)) {
        //println(s"Only one entry for audioTag ${fps.head.audioTag} md5: ${fps.head.md5}, dropping chromaprint")
        fps.seq.map(fp => fp.copy(audioChromaprint = "", audioChromaprintKey = 0L, fp = null))
      } else fps.seq
    }.seq.toBuffer

  chromaprint.clearCaches()

  def audioFingerPrintComponents(): Iterable[Seq[(String, List[String])]] = {
    // precompute per-audioTag data that doesn't change across passes
    val audioByAudioTags = filteredAudioFingerprints.map(e =>
      (e.audioTag, e)).groupMap(_._1)(_._2).par.map { case (k, v) => k -> v.distinct }.seq
    var audioTagData = audioByAudioTags.par.map { case (audioTag, entries) =>
      val hashes = entries.map(_.md5).distinct.sorted.toList
      (audioTag, hashes)
    }.seq
    // group audioTags into connected components by shared md5s
    // audioTags in different components touch disjoint md5 sets and can run in parallel
    val audioTagKeys = audioTagData.map(_._1).toArray
    val parent = mutable.Map[String, String]()
    def find(x: String): String = {
      var r = x
      while (parent.getOrElse(r, r) != r) r = parent.getOrElse(r, r)
      var c = x
      while (c != r) { val n = parent.getOrElse(c, c); parent(c) = r; c = n }
      r
    }
    def union(a: String, b: String): Unit = { parent(find(a)) = find(b) }
    // build mapping: md5 -> first audioTag that uses it, then union subsequent audioTags
    val md5FirstTag = mutable.Map[String, String]()
    for ((audioTag, hashes) <- audioTagData; h <- hashes) {
      md5FirstTag.get(h) match {
        case Some(first) => union(audioTag, first)
        case None => md5FirstTag(h) = audioTag
      }
    }
    val audioTagDataMap = audioTagData.map(t => (t._1, t)).toMap
    audioTagKeys.groupBy(find).values.par.map { tags =>
      tags.map(audioTagDataMap).toSeq
        .groupBy(_._2.toSet)
        .valuesIterator
        .map(_.head)
        .toSeq
    }.seq
  }

  val rawComponents = audioFingerPrintComponents()

  val fpsByMd5 = filteredAudioFingerprints.par.groupBy(_.md5)

  val allSubsongDataByMd5 = fpsByMd5.map { case (md5, fps) =>
    md5 -> fps.seq.groupBy(_.normalizedSubsong).toSeq.sortBy(_._1).map { case (_, subsongFps) =>
      val tags = subsongFps.map(_.audioTag).distinct
      (tags, subsongFps)
    }.toArray
  }.seq.toMap

  val knownMedleyMd5s = ConcurrentHashMap.newKeySet[String]()
  val fullMatchPairs = ConcurrentHashMap.newKeySet[(String, String)]()

  inline def pairKey(k1: Long, k2: Long): Long = {
    val lo = if (k1 < k2) k1 else k2
    val hi = if (k1 < k2) k2 else k1
    (lo << 32) | (hi & 0xFFFFFFFFL)
  }

  inline def pairSeed(k1: Long, k2: Long): Long = if (k1 < k2) k1 else k2

  val rawDuplicatesForTag = {
    val pool = threadpools.workerPool
    val compCaches = new ConcurrentHashMap[Int, LockFreeLongByteMap]()
    val compRemaining = new ConcurrentHashMap[Int, AtomicInteger]()
    val PAIR_LF = 0.83f
    val PAIR_ALPHA = 0.80
    val PAIR_MAX_BUCKETS = 65536L
    def slotsPerBucket(capacity: Int): Int = {
      var n = 16
      while (n.toDouble < Math.ceil(capacity.toDouble / PAIR_LF)) n <<= 1
      n
    }
    val PAIR_INIT_DIV = 2L
    val PAIR_MAX_INIT_MIB = 2560L
    val COOC_MAX_WORK = 16L << 20
    def sizeFor(basis: Long): (Int, Int, Int) = {
      var bestTotal = Long.MaxValue
      var buckets = 8
      var capacity = 8
      var n = 16L
      while (n <= (1L << 22)) {
        val perBucketCap = (n.toDouble * PAIR_ALPHA).toLong
        if (perBucketCap >= 1) {
          val b = (basis + perBucketCap - 1) / perBucketCap
          if (b >= 8 && b <= PAIR_MAX_BUCKETS) {
            val perBucket = Math.max(1L, (basis + b - 1) / b)
            if (perBucket <= (n.toDouble * PAIR_LF).toLong && slotsPerBucket(perBucket.toInt) == n) {
              val t = b * n
              if (t < bestTotal) { bestTotal = t; buckets = b.toInt; capacity = Math.max(8, perBucket.toInt) }
            }
          }
        }
        n <<= 1
      }
      var initCapacity = Math.max(8, (capacity / PAIR_INIT_DIV).toInt)
      while (initCapacity > 8 && slotsPerBucket(initCapacity).toLong * buckets * 9L > PAIR_MAX_INIT_MIB * 1048576L) {
        initCapacity = Math.max(8, initCapacity / 2)
      }
      (buckets, capacity, initCapacity)
    }
    val compData = rawComponents.toSeq.sortBy(-_.size).zipWithIndex.par.map { (component, idx) =>
      val componentId = idx + 1
      val groups = component.groupBy(_._2.toSet).values.toList
      val maxHashes = component.iterator.map(_._2.size).max.toLong
      val (distinctHashes, distinctHashArr, hashGroupCnt) = {
        val all = component.iterator.flatMap(_._2).map(java.lang.Long.parseUnsignedLong(_, 16)).toArray
        Arrays.sort(all)
        val cnts = new Array[Int](all.length)
        var d = 0
        var i = 0
        while (i < all.length) {
          if (i == 0 || all(i) != all(i - 1)) { all(d) = all(i); cnts(d) = 1; d += 1 }
          else cnts(d - 1) += 1
          i += 1
        }
        (d.toLong, Arrays.copyOf(all, d), Arrays.copyOf(cnts, d))
      }
      val groupsArr = groups.toArray
      val groupKeys = groupsArr.map(_.head._2.map(java.lang.Long.parseUnsignedLong(_, 16)).toArray)
      val (coOff, coList) = {
        val off = new Array[Int](distinctHashArr.length + 1)
        var i = 0
        while (i < distinctHashArr.length) { off(i + 1) = off(i) + hashGroupCnt(i); i += 1 }
        val list = new Array[Int](off(distinctHashArr.length))
        val fill = Arrays.copyOf(off, off.length)
        var g = 0
        while (g < groupsArr.length) {
          val keys = groupKeys(g)
          var hi = 0
          while (hi < keys.length) {
            val idx = Arrays.binarySearch(distinctHashArr, keys(hi))
            list(fill(idx)) = g
            fill(idx) += 1
            hi += 1
          }
          g += 1
        }
        (off, list)
      }
      val coocWork = {
        var w = 0L
        var i = 0
        while (i < hashGroupCnt.length) { val c = hashGroupCnt(i).toLong; w += c * (c - 1) / 2; i += 1 }
        w
      }
      val coocPairs =
        if (coocWork > COOC_MAX_WORK || groupsArr.length >= 46341) -1L // 46341^2 >= 2^31
        else {
          val g = groupsArr.length
          val keys = new Array[Int](coocWork.toInt)
          var w = 0
          var hi = 0
          while (hi < distinctHashArr.length) {
            val end = coOff(hi + 1)
            var a = coOff(hi)
            while (a < end) {
              val ga = coList(a) * g
              var b = a + 1
              while (b < end) { keys(w) = ga + coList(b); w += 1; b += 1 }
              a += 1
            }
            hi += 1
          }
          Arrays.sort(keys)
          var pairs = 0L
          var i = 0
          while (i < keys.length) {
            var j = i + 1
            while (j < keys.length && keys(j) == keys(i)) j += 1
            val k = j - i
            pairs += k.toLong * (k - 1) / 2
            i = j
          }
          pairs
        }
      val useCache = component.size > 1 && coocPairs != 0L
      val groupCoBounds: Array[Array[Int]] =
        Array.tabulate(groupsArr.length) { g =>
          val keys = groupKeys(g)
          val b = new Array[Int](keys.length * 2)
          var hi = 0
          while (hi < keys.length) {
            val idx = Arrays.binarySearch(distinctHashArr, keys(hi))
            b(hi * 2) = coOff(idx)
            b(hi * 2 + 1) = coOff(idx + 1)
            hi += 1
          }
          b
        }
      var groupPairSum = 0L
      var maxGroupHashes = 0L
      for (group <- groups) {
        val h = group.head._2.size.toLong
        groupPairSum += h * (h - 1) / 2
        if (h > maxGroupHashes) maxGroupHashes = h
      }
      val totalPairs = Math.max(1L, Math.max(groupPairSum / 2, maxGroupHashes * (maxGroupHashes - 1) / 2))
      val (buckets, capacity, initCapacity) =
        sizeFor(if (coocPairs >= 0L) Math.max(1L, coocPairs) else totalPairs)
      if (useCache) {
        compRemaining.put(componentId, new AtomicInteger(groups.size))
      }
      (componentId, useCache, initCapacity, buckets, capacity, groupsArr, groupCoBounds, coList)
    }.seq
    val tasks = compData.flatMap { case (componentId, useCache, initCapacity, buckets, capacity, groupsArr, coBounds, coList) =>
      groupsArr.zip(coBounds).map { case (group, cb) => (componentId, useCache, initCapacity, buckets, capacity, group, cb, coList) }
    }.sortBy(-_._6.head._2.size)
    val futures = tasks.map { case (componentId, useCache, initCapacity, buckets, capacity, group, coBounds, coList) =>
      pool.submit(new Callable[Seq[(String, Map[String, Set[String]])]] {
        def call(): Seq[(String, Map[String, Set[String]])] = {
          val representative = group.head
          val hashes = representative._2
          val pairResults =
            if (useCache) compCaches.computeIfAbsent(componentId, _ =>
              new LockFreeLongByteMap(
                buckets,
                slotsPerBucket(initCapacity).toInt,
                Byte.MinValue
              )
            )
            else null
          val hashesArr = hashes.toArray
          val hashKeys = hashesArr.map(java.lang.Long.parseUnsignedLong(_, 16))
          val numHashes = hashesArr.length
          val parent = Array.tabulate(numHashes)(identity)
          def find(x: Int): Int = {
            var r = x
            while (parent(r) != r) r = parent(r)
            var c = x
            while (c != r) { val n = parent(c); parent(c) = r; c = n }
            r
          }
          def union(a: Int, b: Int): Unit = { parent(find(a)) = find(b) }
          val subsetsOf = mutable.Map[Int, Buffer[Int]]()

          val tagIds = new mutable.HashMap[String, Int]()
          def tagId(t: String): Int = {
            var id = tagIds.getOrElse(t, -1)
            if (id < 0) { id = tagIds.size; tagIds.put(t, id) }
            id
          }
          var maxValid = 0
          val validByHash = Array.tabulate(numHashes) { idx =>
            val valid = allSubsongDataByMd5(hashesArr(idx)).filter(_._2.exists(_.effectiveAudioBytes > 0))
            if (valid.length > maxValid) maxValid = valid.length
            val bytes = new Array[Int](valid.length)
            var zb = 0
            while (zb < valid.length) { bytes(zb) = valid(zb)._2.head.effectiveAudioBytes; zb += 1 }
            Arrays.sort(bytes)
            val tags = valid.map(_._1.toArray)
            val subs = new Array[Array[Int]](valid.length)
            var idxLen = 0
            zb = 0
            while (zb < valid.length) {
              val ids = tags(zb).map(tagId).distinct
              subs(zb) = ids
              idxLen += ids.length
              zb += 1
            }
            val index = new Array[Long](idxLen)
            var wp = 0
            zb = 0
            while (zb < valid.length) {
              val ids = subs(zb)
              var zc = 0
              while (zc < ids.length) { index(wp) = (ids(zc).toLong << 32) | zb.toLong; wp += 1; zc += 1 }
              zb += 1
            }
            Arrays.sort(index)
            new SubsongTags(valid, tags, bytes, subs, index)
          }
          val idToTag = new Array[String](tagIds.size)
          for ((t, id) <- tagIds) idToTag(id) = t
          val matchedLarger = new Array[Boolean](maxValid)

          def pairCoOccurs(i1: Int, i2: Int): Boolean = {
            var a = coBounds(i1 * 2)
            val aEnd = coBounds(i1 * 2 + 1)
            var b = coBounds(i2 * 2)
            val bEnd = coBounds(i2 * 2 + 1)
            var shared = 0
            while (a < aEnd && b < bEnd && shared < 2) {
              val ga = coList(a)
              val gb = coList(b)
              if (ga == gb) {
                shared += 1
                a += 1
                b += 1
              }
              else if (ga < gb) a += 1
              else b += 1
            }
            shared >= 2
          }
          def eligible(i1: Int, i2: Int): Boolean = {
            val ia = validByHash(i1).index
            val ib = validByHash(i2).index
            var p = 0
            var q = 0
            var ok = false
            while (p < ia.length && q < ib.length && !ok) {
              val ta = ia(p) >>> 32
              val tb = ib(q) >>> 32
              if (ta == tb) ok = true
              else if (ta < tb) { p += 1; while (p < ia.length && (ia(p) >>> 32) == ta) p += 1 }
              else { q += 1; while (q < ib.length && (ib(q) >>> 32) == tb) q += 1 }
            }
            ok
          }
          def pairDuplicateResult(i1: Int, i2: Int, cacheable: Boolean): Int = {
            val k1 = hashKeys(i1)
            val k2 = hashKeys(i2)
            val key = pairKey(k1, k2)
            val seed = pairSeed(k1, k2)
            val cached =
              if (!cacheable) Byte.MinValue
              else pairResults.get(key, seed)
            if (cached != Byte.MinValue) cached & 0xFF
            else {
              val A = validByHash(i1)
              val B = validByHash(i2)
              val validA = A.valid
              val validB = B.valid
              val bytesA = A.bytes
              val bytesB = B.bytes
              val lenA = validA.length
              val lenB = validB.length

              var duplicate = true

              val smaller = if (lenA <= lenB) validA else validB
              val larger = if (lenA <= lenB) validB else validA
              val smallerTagIds = if (lenA <= lenB) A.subs else B.subs
              val largerTagIds = if (lenA <= lenB) B.subs else A.subs

              val smallerAudioBytes = if (lenA <= lenB) bytesA else bytesB
              val largerAudioBytes = if (lenA <= lenB) bytesB else bytesA
              var isSubset = smallerAudioBytes.length <= largerAudioBytes.length
              var si = 0
              var li = 0
              while (isSubset && si < smallerAudioBytes.length) {
                while (li < largerAudioBytes.length && largerAudioBytes(li) < smallerAudioBytes(si)) li += 1
                if (li == largerAudioBytes.length || largerAudioBytes(li) != smallerAudioBytes(si)) isSubset = false
                else { li += 1; si += 1 }
              }

              val requiredStrictMatches = if (isSubset || smaller.length == 1) 1 else 2

              var i = 0
              var strictMatchCount = 0
              while (i < smaller.length && strictMatchCount < requiredStrictMatches && (smaller.length - i) + strictMatchCount >= requiredStrictMatches) {
                val cmpFps = smaller(i)._2
                val cmpTagIds = smallerTagIds(i)
                var k = 0
                val initialMatchCount = strictMatchCount

                while (k < larger.length && strictMatchCount == initialMatchCount) {
                  val (_, fps) = larger(k)
                  val largerTagIdsK = largerTagIds(k)
                  var ti = 0
                  var commonId = -1
                  while (ti < cmpTagIds.length && commonId < 0) {
                    val t = cmpTagIds(ti)
                    var li = 0
                    while (li < largerTagIdsK.length && largerTagIdsK(li) != t) li += 1
                    if (li < largerTagIdsK.length) commonId = t
                    ti += 1
                  }

                  if (commonId >= 0) {
                    val commonTag = idToTag(commonId)
                    var ci = 0
                    var cmpFp: AudioFingerprint = null
                    while (ci < cmpFps.length && cmpFp == null) {
                      val f = cmpFps(ci)
                      if (f.audioTag == commonTag) cmpFp = f
                      ci += 1
                    }
                    var fi = 0
                    var fp: AudioFingerprint = null
                    while (fi < fps.length && fp == null) {
                      val f = fps(fi)
                      if (f.audioTag == commonTag) fp = f
                      fi += 1
                    }

                    if (cmpFp != null && fp != null) {
                      var isStrictMatch = true
                      if (cmpFp.audioMd5 == fp.audioMd5 && cmpFp.audioMd5.nonEmpty) {
                        // duplicate = true
                      } else if (cmpFp.audioChromaprintKey != 0L && fp.audioChromaprintKey != 0L && cmpFp.audioChromaprintKey != fp.audioChromaprintKey) {
                        val cmpseconds = cmpFp.effectiveAudioBytes.toDouble / persecondbytes
                        val fpseconds = fp.effectiveAudioBytes.toDouble / persecondbytes
                        val threshold = if (cmpseconds >= 10 && fpseconds >= 10) Math.max(0.999 - 0.01 * Math.min(cmpseconds - 10, fpseconds - 10), 0.82) else 0.999
                        val similarity = chromaSimilarityFPs(cmpFp.fp, fp.fp)
                        if (similarity < threshold) {
                          isStrictMatch = false
                        }
                      } else if (cmpFp.audioHash != fp.audioHash) {
                        isStrictMatch = false
                      }
                      if (isStrictMatch && !matchedLarger(k)) {
                        strictMatchCount += 1
                        matchedLarger(k) = true
                      }
                    } // end cmpFp != null && fp != null
                  }
                  k += 1
                  while (k < larger.length && matchedLarger(k)) k += 1
                }
                i += 1
              }
              if (strictMatchCount > 0) Arrays.fill(matchedLarger, 0, larger.length, false)
              if (duplicate && strictMatchCount < requiredStrictMatches) {
                duplicate = false
              }
              val packed = (if (duplicate) 0x80 else 0) | (math.min(strictMatchCount, 127) & 0x7F)
              if (cacheable) pairResults.put(key, packed.toByte, seed)
              packed
            }
          }

          var cmpIdx = 0
          while (cmpIdx < numHashes) {
            val cmpHash = hashesArr(cmpIdx)
            val cmpLen = math.min(validByHash(cmpIdx).valid.length, 127)

            var j = cmpIdx + 1
            while (j < numHashes) {
              if (find(cmpIdx) != find(j)) {
                if (eligible(cmpIdx, j)) {
                  val cacheable = useCache && pairCoOccurs(cmpIdx, j)
                  val subHash = hashesArr(j)
                  val subLen = math.min(validByHash(j).valid.length, 127)
                  val packed = pairDuplicateResult(cmpIdx, j, cacheable)
                  val duplicate = (packed & 0x80) != 0
                  val strictMatchCount = packed & 0x7F
                  if (duplicate) {
                    if (cmpLen == subLen) {
                      union(cmpIdx, j)
                      if (strictMatchCount == cmpLen) {
                        fullMatchPairs.add((cmpHash, subHash))
                        fullMatchPairs.add((subHash, cmpHash))
                      }
                    } else {
                      val largerIdx = if (cmpLen > subLen) cmpIdx else j
                      val smallerIdx = if (cmpLen > subLen) j else cmpIdx
                      subsetsOf.getOrElseUpdate(largerIdx, Buffer.empty) += smallerIdx
                    }
                  }
                }
              }
              j += 1
            }
            cmpIdx += 1
          }

          for ((largerIdx, subsets) <- subsetsOf) {
            val largerHash = hashesArr(largerIdx)
            var isMedley = knownMedleyMd5s.contains(largerHash)
            if (!isMedley) {
              var aIdx = 0
              while (aIdx < subsets.length && !isMedley) {
                val a = subsets(aIdx)
                var bIdx = aIdx + 1
                while (bIdx < subsets.length && !isMedley) {
                  val b = subsets(bIdx)
                  if (find(a) != find(b)) {
                    if (eligible(a, b)) {
                      val cacheable = useCache && pairCoOccurs(a, b)
                      if ((pairDuplicateResult(a, b, cacheable) & 0x80) == 0) {
                        isMedley = true
                      }
                    }
                  }
                  bIdx += 1
                }
                aIdx += 1
              }
            }
            for (sub <- subsets) union(largerIdx, sub)
            if (isMedley) {
              System.err.println(s"INFO: Detected medley for $largerHash with subsets: ${subsets.map(hashesArr).mkString(", ")}")
              knownMedleyMd5s.add(largerHash)
            }
          }

          val lenMap = hashesArr.indices.map(i => hashesArr(i) -> validByHash(i).valid.length).toMap
          val dupSets = hashesArr.indices.groupBy(find).values.map(_.map(hashesArr).toSet).toSeq
          val dupMap = dupSets.flatMap(set => set.map(h => h -> set.filter(x => lenMap(h) >= lenMap(x)))).toMap
          if (useCache && compRemaining.get(componentId).decrementAndGet() == 0) {
            compCaches.remove(componentId)
            compRemaining.remove(componentId)
          }
          group.map { case (audioTag, _) => (audioTag, dupMap) }
        }
      })
    }
    val results = futures.map(_.get)
    results.flatten.toMap
  }

  val _duplicatesForTag: Map[String, Map[(String, Boolean), Set[String]]] = rawDuplicatesForTag.par.map { case (tag, dupMap) =>
    tag -> dupMap.flatMap { case (k, v) =>
      Seq(
        (k, true) -> {
          if (knownMedleyMd5s.contains(k))
            v.filter(m => m == k || (knownMedleyMd5s.contains(m) && fullMatchPairs.contains((k, m))))
          else
            v.filterNot(knownMedleyMd5s.contains)
        },
        (k, false) -> v
      )
    }
  }.seq

  val audioByPlayerAndMd5 = filteredAudioFingerprints.par.groupBy(e => (e.player, e.md5))
    .map { case (k, fps) => k -> fps.seq.sortBy(_.normalizedSubsong).distinct }.seq
  
  val _duplicateSubsongsByPlayerAndMd5 = songlengths.db.sortBy(_.md5).par.flatMap(e => {
    val md5 = e.md5.take(12)
    val duplicates = mutable.SortedSet[Int]()
    val fingerprints = audioByPlayerAndMd5.get((e.player, md5)).getOrElse(Buffer.empty)
    if (fingerprints.nonEmpty) {
      val filtered = fingerprints.filter(f => f.effectiveAudioBytes > 0).distinctBy(f => (f.subsong, f.audioTag))
      val grouped = (
        if (filtered.forall(e => filtered.head.effectiveAudioBytes > persecondbytes * 12 && e.effectiveAudioBytes > persecondbytes * 12 && e.audioBytes == filtered.head.audioBytes)) filtered.groupBy(_.audioBytes)
        else filtered.groupBy(_.audioTag)
      ).view.mapValues(_.distinct).toMap
      val audioTags = filtered.map(f => (f.subsong, f.audioTag)).distinct.groupBy(_._1).view.mapValues(_.map(_._2).sorted.distinct).toMap
      if (!audioTags.values.forall(_.size <= 2)) {
        System.err.println(s"WARN: inconsistent audio tags for md5: $md5 player: ${e.player} format: ${e.format} audioTags: ${audioTags}")
      }
      val audioTagsIdentical = filtered.forall(_.effectiveAudioBytes > persecondbytes * 12) && (
        (audioTags.values.forall(_ == audioTags.head._2) && e.subsongs.size > 2) ||
        audioTags.values.forall(_ == audioTags.head._2) && filtered.forall(_.audioBytes == filtered.head.audioBytes)
      )
      val baseThreshold = if (audioTagsIdentical) 0.9 else 0.99
      try {
        assert(grouped.values.forall(group => group.map(_.subsong).sorted == group.map(_.subsong)))
      } catch {
        case ex: Throwable =>
          System.err.println(s"ERROR: md5: ${md5} player: ${e.player} format: ${e.format} filtered: ${filtered} grouped: ${grouped.values.map(_.map(_.subsong).mkString(",")).mkString(";")}")
          throw ex
      }
      for ((_, group) <- grouped) {
        var remaining = group
        while (remaining.nonEmpty) {
          val cmp = remaining.head
          remaining = remaining.filterNot(_.subsong == cmp.subsong)
          for (se <- remaining) {
            var duplicate = true
            // XXX audioChromaprint may differ even if md5 is same
            if (cmp.audioMd5 == se.audioMd5) {
              duplicate = true
            } else if (se.audioChromaprintKey != 0L && cmp.audioChromaprintKey != 0L && se.audioChromaprintKey != cmp.audioChromaprintKey) {
              val threshold = (if (audioTags(se.subsong) != audioTags(cmp.subsong)) 0.995 else baseThreshold)
              val similarity = chromaSimilarityFPs(cmp.fp, se.fp)
              if (similarity < threshold) {
                duplicate = false
              }
            } else if (se.audioHash != cmp.audioHash) {
              duplicate = false
            }
            if (duplicate) {
              duplicates += se.subsong
            }
          }
          remaining = remaining.filterNot(se => duplicates.contains(se.subsong))
        }
      }
      if (duplicates.nonEmpty && e.subsongs.size > duplicates.size) {
        System.err.println(s"INFO: md5: $md5 has duplicate subsongs: ${duplicates.mkString(",")} player: ${e.player} format: ${e.format}")
      }
    }
    if (duplicates.nonEmpty) Some((e.player, md5) -> duplicates) else None
  }).seq.toMap

  val _audioHashesByMd5 = fpsByMd5.map { case (md5, fps) =>
    md5 -> fps.seq.sortBy(_.normalizedSubsong).map(_.audioHash).distinct
  }.seq.toMap

  val _subsongCountsByMd5 = allSubsongDataByMd5.par.map { case (md5, subsongs) =>
    md5 -> subsongs.count(_._2.exists(_.effectiveAudioBytes > 0))
  }.seq.toMap

  val _components = rawComponents.par.map { _.map { case (audioTag, hashes) =>
    val groupedHashes = hashes.groupBy(h => _subsongCountsByMd5(h)).map { case (count, hashes) => (count, hashes.sorted) }.toSeq.sortBy(-_._1)
    //println(s"DEBUG: component audioTag: ${audioTag} groupedHashes: ${groupedHashes.map { case (count, hashes) => s"${count}:${hashes.mkString(",")}" }.mkString(";")}")
    (audioTag, groupedHashes)
  }}.seq

  (_audioHashesByMd5, _components, _duplicatesForTag, _duplicateSubsongsByPlayerAndMd5)
}
