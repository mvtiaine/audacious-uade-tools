// SPDX-License-Identifier: GPL-2.0-or-later AND CC-PDM-1.0
// SPDX-AI-Disclosure: ai-assisted
// Copyright (C) 2025-2026 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.lz4:lz4-java:1.8.1

import java.util.Arrays
import java.util.Base64
import java.util.concurrent.ConcurrentHashMap
import net.jpountz.xxhash.XXHashFactory

import chromaanalysis._
import chromaprint._

val xxh64 = XXHashFactory.fastestInstance().hash64()
val xxh64Seed = 0

def xxh64Hash(bytes: Array[Byte]): Long =
  xxh64.hash(bytes, 0, bytes.length, xxh64Seed)

def chromaprintHash(bytes: Array[Byte]): Long =
  xxh64Hash(bytes)

def chromaprintHash(chromaprint: String): Long =
  chromaprintHash(Base64.getUrlDecoder.decode(chromaprint))

def simHashHash(simHash: BigInt): Long =
  xxh64Hash(simHash.toByteArray)

final class FP (val algo: Int, val data: Array[Int], val key: Long = 0L) {
  val length: Int = data.length

  lazy val isSilent: Boolean = isSilentFingerprint(data)
  lazy val isRepetitive: Boolean = isRepetitiveFingerprint(data)
  lazy val isNoisy: Boolean = isNoisyFingerprint(data)
  lazy val isBad: Boolean = isBadFingerprint(data)
  lazy val isUnusable: Boolean = !isBad && isUnusableFingerprint(data)

  lazy val contentBounds: (Int, Int) = {
    val numChunks = (length + chunkSize - 1) / chunkSize
    var s = 0
    while (s < numChunks && isSilentChunk(data, if (s == numChunks - 1) length - s * chunkSize else chunkSize, s * chunkSize)) s += 1
    var e = numChunks - 1
    while (e >= s && isSilentChunk(data, if (e == numChunks - 1) length - e * chunkSize else chunkSize, e * chunkSize)) e -= 1
    if (s > e) (0, 0) else (s * chunkSize, math.min(length, (e + 1) * chunkSize))
  }

  // per-chunk silence mask; only the similarity correction asks for it, so a pair
  // that cannot match never pays for it
  lazy val silentChunks: Array[Boolean] = {
    val numChunks = (length + chunkSize - 1) / chunkSize
    val m = new Array[Boolean](numChunks)
    var c = 0
    while (c < numChunks) {
      val st = c * chunkSize
      m(c) = isSilentChunk(data, math.min(chunkSize, length - st), st)
      c += 1
    }
    m
  }

  // words in non-silent chunks only; silent chunks in the middle do not count, so a
  // replay that is mostly silence with a few seconds of audio yields a small value
  lazy val liveWords: Int = {
    val numChunks = (length + chunkSize - 1) / chunkSize
    var live = 0
    var c = 0
    while (c < numChunks) {
      val start = c * chunkSize
      if (!isSilentChunk(data, math.min(chunkSize, length - start), start)) live += math.min(chunkSize, length - start)
      c += 1
    }
    live
  }

  // audioBytes scaled to the non-silent content of this fingerprint
  def effectiveBytes(audioBytes: Int): Int =
    if (length == 0) audioBytes
    else (audioBytes.toLong * liveWords / length).toInt

  // hashCode/equals compare the array contents, not identity
  override lazy val hashCode: Int = algo * 31 + Arrays.hashCode(data)
  override def equals(obj: Any): Boolean = obj match {
    case other: FP => algo == other.algo && length == other.length && Arrays.equals(data, other.data)
    case _ => false
  }
}
val fpCache = new ConcurrentHashMap[Long, FP](500_000)

def clearCaches(): Unit = {
  fpCache.clear()
}

def cacheChromaprint(chromaprint: String): (Long, FP) = {
  val bytes = Base64.getUrlDecoder.decode(chromaprint)
  val hash = chromaprintHash(bytes)
  val fp = fpCache.computeIfAbsent(hash, _ => {
    val Right(algo, data) = FingerprintDecompressor(bytes) : @unchecked
    new FP(algo, data, hash)
  })
  (hash, fp)
}

// decode without caching (for one-off comparisons in find_dupes.sc/audio_match.sc)
def decodeChromaprintUncached(chromaprint: String): FP = {
  val Right(algo, data) = FingerprintDecompressor(chromaprint) : @unchecked
  new FP(algo, data)
}

// the duplicate match threshold for two subsongs of the given effective (non-silent)
// lengths in seconds: 0.999 unless both are at least 10s, then easing towards 0.82
// with their length
def fpThreshold(qSec: Double, rSec: Double): Double =
  if (qSec >= 10 && rSec >= 10) Math.max(0.999 - 0.01 * Math.min(qSec - 10, rSec - 10), 0.82) else 0.999

// similarity between two already-decoded FPs.
def chromaSimilarityFPs(fp1: FP, fp2: FP): Double = {
  assert(fp1.algo == fp2.algo)
  val (s1, e1) = fp1.contentBounds
  val (s2, e2) = fp2.contentBounds
  val raw = chromaSimilarityFast(fp1.algo, s1, e1, fp1.data, fp2.algo, s2, e2, fp2.data)
  // Silence matches silence: two mostly-silent renderings score high without sharing
  // music. Re-score excluding both-silent words, but only when most of the overlap was
  // silence-vs-silence. min() keeps the correction from creating a match raw did not.
  if (raw <= 0.6) raw
  else {
    val (content, silentShare) = chromaSimilarityNoSilence(fp1, fp2, 3)
    if (silentShare * 3 <= 1.0) raw else math.min(raw, content)
  }
}

// The same offset scan excluding words silent on both sides, plus the skipped fraction.
// maxOffset must cover what the raw scan tried.
def chromaSimilarityNoSilence(fp1: FP, fp2: FP, maxOffset: Int): (Double, Double) = {
  val (s1, e1) = fp1.contentBounds
  val (s2, e2) = fp2.contentBounds
  val d1 = fp1.data
  val d2 = fp2.data
  val m1 = fp1.silentChunks
  val m2 = fp2.silentChunks
  val minOverlap = math.min(8, math.min(e1 - s1, e2 - s2) / 2)
  var best = -1.0
  var bestSilentShare = 0.0
  var off = -maxOffset
  while (off <= maxOffset) {
    val iStart = math.max(s1, s2 + off)
    val iEnd = math.min(e1, e2 + off)
    var totalScore = 0
    var overlap = 0
    var skipped = 0
    var i = iStart
    while (i < iEnd) {
      if (m1(i / chunkSize) && m2((i - off) / chunkSize)) skipped += 1
      else {
        totalScore += 32 - Integer.bitCount(d1(i) ^ d2(i - off))
        overlap += 1
      }
      i += 1
    }
    if (overlap >= minOverlap && overlap > 0) {
      val similarity = totalScore.toDouble / (overlap * 32.0)
      if (similarity > best) {
        best = similarity
        bestSilentShare = skipped.toDouble / (skipped + overlap)
      }
    }
    off += 1
  }
  if (best < 0.0) (0.0, 1.0) else (best, bestSilentShare)
}

def chromaSimilarityFast(
  algo1: Int,
  start1: Int,
  end1: Int,
  data1: Array[Int],
  algo2: Int,
  start2: Int,
  end2: Int,
  data2: Array[Int],
  fuzziness: Int = 3
): Double = {
  if (algo1 != algo2) {
    return 0.0
  }

  val minOverlap = math.min(8, math.min(end1 - start1, end2 - start2) / 2)
  var maxSimilarity = 0.0
  var zeroSimilarity = -1.0

  var pass = 0
  while (pass < 2 && (maxSimilarity <= 0.0 || (maxSimilarity > 0.6 && maxSimilarity < 0.99))) {
    var oi = if (pass == 0) 0 else 1
    var prevSimilarity = zeroSimilarity
    while (oi <= fuzziness && (maxSimilarity <= 0.0 || (maxSimilarity > 0.6 && maxSimilarity < 0.99))) {
      val offset = if (pass == 0) oi else -oi
      val iStart = Math.max(start1, start2 - offset)
      val iEnd = Math.min(end1, end2 - offset)
      var totalScore = 0
      var overlap = 0
      var i = iStart

      while (i < iEnd) {
        val v1 = data1(i)
        val j = i + offset
        val v2 = data2(j)
        totalScore += (32 - Integer.bitCount(v1 ^ v2))
        overlap += 1
        i += 1
      }

      if (overlap >= minOverlap && overlap > 0) {
        val similarity = totalScore.toDouble / (overlap * 32.0)
        if (similarity < prevSimilarity) {
          oi = fuzziness // break early
        } else {
          prevSimilarity = similarity
          if (similarity > maxSimilarity) {
            maxSimilarity = similarity
          }
        }
      }
      if (oi == 0) {
        zeroSimilarity = maxSimilarity
      }
      oi += 1
    }
    pass += 1
  }

  maxSimilarity
}
