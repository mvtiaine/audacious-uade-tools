// SPDX-License-Identifier: MIT
// Copyright (C) 2025-2026 Matti Tiainen <mvtiaine@cc.hut.fi>
// see below for further copyrights

//> using dep org.lz4:lz4-java:1.8.0

import java.util.concurrent.ConcurrentHashMap
import java.util.Base64
import net.jpountz.xxhash.XXHashFactory
import scala.collection.mutable

// number of 32-bit words per silence/content scan window (was the FpChunker chunk size)
val chunkSize = 64

def isSilentChunk(data: Array[Int], len: Int, offset: Int = 0): Boolean = {
  var totalBits = 0
  var zeroCount = 0
  val uniqueValuesSet = scala.collection.mutable.HashSet[Int]()
  var i = 0
  while (i < len) {
    val v = data(offset + i)
    totalBits += Integer.bitCount(v)
    if (v == 0) zeroCount += 1
    uniqueValuesSet += v
    i += 1
  }
  val totalPossibleBits = len * 32
  val setBitRatio = totalBits.toDouble / totalPossibleBits
  val uniqueValues = uniqueValuesSet.size
  val repetitionRatio = uniqueValues.toDouble / len
  val zeroRatio = zeroCount.toDouble / len
  setBitRatio < 0.005 || zeroRatio > 0.9 || (setBitRatio < 0.02 && repetitionRatio < 0.05) || uniqueValues <= 2
}

def isSilentFingerprint(data: Array[Int]): Boolean = {
  if (data.isEmpty) return true
  var start = 0
  while (start < data.length) {
    val len = math.min(chunkSize, data.length - start)
    if (!isSilentChunk(data, len, start)) return false
    start += chunkSize
  }
  true
}

final class FP (val algo: Int, val data: Array[Int]) {
  val length: Int = data.length
  lazy val isSilent: Boolean = isSilentFingerprint(data)

  lazy val contentBounds: (Int, Int) = {
    val numChunks = (length + chunkSize - 1) / chunkSize
    var s = 0
    while (s < numChunks && isSilentChunk(data, if (s == numChunks - 1) length - s * chunkSize else chunkSize, s * chunkSize)) s += 1
    var e = numChunks - 1
    while (e >= s && isSilentChunk(data, if (e == numChunks - 1) length - e * chunkSize else chunkSize, e * chunkSize)) e -= 1
    if (s > e) (0, 0) else (s * chunkSize, math.min(length, (e + 1) * chunkSize))
  }

  // hashCode and equals omit deep array checks as they're not used in deduplication anymore
  // but kept valid for exact matching if needed
  override val hashCode: Int = algo * 31 + java.util.Arrays.hashCode(data)
  override def equals(obj: Any): Boolean = obj match {
    case other: FP => algo == other.algo && length == other.length && java.util.Arrays.equals(data, other.data)
    case _ => false
  }
}
val fpCache = new ConcurrentHashMap[String, FP]()

// xxhash64 of the base64 decoded chromaprint bytes, used as a compact cache key
// so the full base64 chromaprint string does not need to be retained in memory
val xxh64 = XXHashFactory.fastestInstance().hash64()
val xxh64Seed = 0

def chromaprintHash(chromaprint: String): String = {
  val bytes = Base64.getUrlDecoder.decode(chromaprint)
  java.lang.Long.toHexString(xxh64.hash(bytes, 0, bytes.length, xxh64Seed))
}

def clearCaches(): Unit = {
  fpCache.clear()
  similarityCache.clear()
}

// decode + cache the chromaprint under its xxhash64 key, returning the compact key
def cacheChromaprint(chromaprint: String): String = {
  val hash = chromaprintHash(chromaprint)
  fpCache.computeIfAbsent(hash, _ => {
    val Right(algo, data) = FingerprintDecompressor(chromaprint) : @unchecked
    new FP(algo, data)
  })
  hash
}

// lookup a previously cached FP by its xxhash64 key (returns null if not cached)
def fpByHash(hash: String): FP = fpCache.get(hash)

/*
def decodeChromaprint(chromaprint: String): FP = {
  val hash = chromaprintHash(chromaprint)
  val cached = fpCache.get(hash)
  if (cached != null) return cached
  fpCache.computeIfAbsent(hash, _ => {
    val Right(algo, data) = FingerprintDecompressor(chromaprint) : @unchecked
    new FP(algo, data)
  })
}
*/

// decode without caching (for one-off comparisons in find_dupes.sc/audio_match.sc)
def decodeChromaprintUncached(chromaprint: String): FP = {
  val Right(algo, data) = FingerprintDecompressor(chromaprint) : @unchecked
  new FP(algo, data)
}

// similarity between two already-decoded FPs, without caching
def chromaSimilarityFPs(fp1: FP, fp2: FP, matchSilence: Boolean = false): Double = {
  assert(fp1.algo == fp2.algo)
  val (s1, e1) = fp1.contentBounds
  val (s2, e2) = fp2.contentBounds
  val sim = chromaSimilarityFast(fp1.algo, s1, e1, fp1.data, fp2.algo, s2, e2, fp2.data)
  if (!matchSilence && (fp1.isSilent || fp2.isSilent)) 0.0 else sim
}

final class StringPair(val a: String, val b: String) {
  override val hashCode: Int = a.hashCode * 31 + b.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case other: StringPair => a == other.a && b == other.b
    case _ => false
  }
}
val similarityCache = new ConcurrentHashMap[StringPair, java.lang.Double]()

def chromaSimilarity(chromaprint1: String, chromaprint2: String, matchSilence: Boolean = false): Double = {
  if (chromaprint1 == chromaprint2) {
    return 1.0
  }
  val isLess = chromaprint1 < chromaprint2
  val cp1 = if (isLess) chromaprint1 else chromaprint2
  val cp2 = if (isLess) chromaprint2 else chromaprint1
  val key = new StringPair(cp1, cp2)
  val cached = similarityCache.get(key)
  val sim: Double = if (cached != null) cached.doubleValue else similarityCache.computeIfAbsent(key, _ => {
    val fp1 = fpByHash(cp1)
    val fp2 = fpByHash(cp2)
    assert(fp1.algo == fp2.algo)
    val (s1, e1) = fp1.contentBounds
    val (s2, e2) = fp2.contentBounds
    chromaSimilarityFast(fp1.algo, s1, e1, fp1.data, fp2.algo, s2, e2, fp2.data)
  })

  if (!matchSilence && (fpByHash(cp1).isSilent || fpByHash(cp2).isSilent)) {
    0.0
  } else {
    sim
  }
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

      val minOverlap = math.min(8, math.min(end1 - start1, end2 - start2) / 2)
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
/*
def chromaSimilarity(
  algo1: Int,
  data1: IndexedSeq[spire.math.UInt],
  algo2: Int,
  data2: IndexedSeq[spire.math.UInt]
): Double = {
  if (algo1 != algo2) {
    return 0.0
  }

  val (shorter, longer) = if (data1.length < data2.length) (data1, data2) else (data2, data1)

  if (shorter.isEmpty) {
    return if (longer.isEmpty) 1.0 else 0.0
  }

  val maxOffset = longer.length - 1
  var maxSimilarity = 0.0

  for (offset <- -(shorter.length - 1) to maxOffset) {
    var currentScore = 0
    var overlap = 0
    for (i <- shorter.indices) {
      val j = i + offset
      if (j >= 0 && j < longer.length) {
        val xorValue = shorter(i) ^ longer(j)
        currentScore += (32 - Integer.bitCount(xorValue.toInt))
        overlap += 1
      }
    }
    if (overlap > 0) {
      val currentSimilarity = currentScore.toDouble / (overlap * 32.0)
      if (currentSimilarity > maxSimilarity) {
        maxSimilarity = currentSimilarity
      }
    }
  }

  maxSimilarity
}

*/
// Chromaprint decoding and SimHash code, with some modifications, from:
// https://github.com/mgdigital/Chromaprint.scala
/*
Copyright (c) 2019 Mike Gibson, https://github.com/mgdigital

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

Original Chromaprint algorithm Copyright (c) Lukáš Lalinský.
*/

// FingerprintDecompressor.scala
//package chromaprint

// mvtiaine: vibe optimized with Claude Sonnet 4 / Claude Opus 4
// mvtiaine: replaced custom/scodec Base64 code with java.util.Base64.Decoder

object FingerprintDecompressor {

  final class DecompressorException(message: String) extends Exception(message)

  def apply(data: String): Either[DecompressorException,(Int, Array[Int])] =
    apply(Base64.getUrlDecoder.decode(data))

  def apply(bytes: IndexedSeq[Byte]): Either[DecompressorException,(Int, Array[Int])] =
    apply(bytes.toArray)

  def apply(bytes: Array[Byte]): Either[DecompressorException,(Int, Array[Int])] =
    if (bytes.length < 5) {
      Left(new DecompressorException("Invalid fingerprint (shorter than 5 bytes)"))
    } else {
      val algorithm: Int = bytes(0).toInt
      val length: Int = ((0xff & bytes(1)) << 16) | ((0xff & bytes(2)) << 8) | (0xff & bytes(3))
      if (algorithm < 0) {
        Left(new DecompressorException("Invalid algorithm"))
      } else if (length < 1) {
        Left(new DecompressorException("Invalid length"))
      } else {
        decompressFingerprint(bytes, 4, algorithm, length)
      }
    }

  private def decompressFingerprint(
    bytes: Array[Byte], bodyOffset: Int, algorithm: Int, length: Int
  ): Either[DecompressorException, (Int, Array[Int])] = {
    // Step 1: decode triplets from body
    val triplets = bytesToTriplets(bytes, bodyOffset, bytes.length)

    // Step 2: scan triplets to find group count and exception count
    var groups = 0
    var tripletsUsed = 0
    var exceptionCount = 0
    while (groups < length && tripletsUsed < triplets.length) {
      val v = triplets(tripletsUsed)
      if (v == 0) groups += 1
      else if (v == 7) exceptionCount += 1
      tripletsUsed += 1
    }
    if (groups < length) {
      return Left(new DecompressorException("Not enough normal bits"))
    }

    // Step 3: decode quintets from remaining bytes
    val quintetByteOffset = bodyOffset + packedTripletSize(tripletsUsed)
    val quintets = bytesToQuintets(bytes, quintetByteOffset, bytes.length)
    if (exceptionCount > quintets.length) {
      return Left(new DecompressorException("Not enough exception bits"))
    }

    // Step 4: single-pass combine + unpack
    val result = new Array[Int](length)
    var resultIdx = 0
    var value = 0
    var lastBit = 0
    var previousValue = 0
    var quintetIdx = 0
    var ti = 0
    while (ti < tripletsUsed) {
      val v = triplets(ti)
      if (v == 0) {
        val finalValue = if (resultIdx == 0) value else value ^ previousValue
        result(resultIdx) = finalValue
        previousValue = finalValue
        resultIdx += 1
        value = 0
        lastBit = 0
      } else {
        val actual = if (v == 7) { val q = quintets(quintetIdx); quintetIdx += 1; v + q } else v
        lastBit += actual
        value |= (1 << (lastBit - 1))
      }
      ti += 1
    }

    Right((algorithm, result))
  }

  private def packedTripletSize(size: Int): Int =
    (size * 3 + 7) >> 3

  private def bytesToTriplets(bytes: Array[Byte], start: Int, end: Int): Array[Int] = {
    val maxTriplets = ((end - start) * 8 + 2) / 3
    val result = new Array[Int](maxTriplets)
    var ri = 0
    var i = start

    while (i < end) {
      val b0 = bytes(i) & 0xff
      result(ri) = b0 & 0x07; ri += 1
      result(ri) = (b0 >> 3) & 0x07; ri += 1

      if (i + 1 < end) {
        val b1 = bytes(i + 1) & 0xff
        result(ri) = ((b0 >> 6) & 0x03) | ((b1 & 0x01) << 2); ri += 1
        result(ri) = (b1 >> 1) & 0x07; ri += 1
        result(ri) = (b1 >> 4) & 0x07; ri += 1

        if (i + 2 < end) {
          val b2 = bytes(i + 2) & 0xff
          result(ri) = ((b1 >> 7) & 0x01) | ((b2 & 0x03) << 1); ri += 1
          result(ri) = (b2 >> 2) & 0x07; ri += 1
          result(ri) = (b2 >> 5) & 0x07; ri += 1
        }
      }
      i += 3
    }

    if (ri < result.length) java.util.Arrays.copyOf(result, ri) else result
  }

  private def bytesToQuintets(bytes: Array[Byte], start: Int, end: Int): Array[Int] = {
    val maxQuintets = ((end - start) * 8 + 4) / 5
    val result = new Array[Int](maxQuintets)
    var ri = 0
    var i = start

    while (i < end) {
      val q0 = bytes(i) & 0xff
      result(ri) = q0 & 0x1f; ri += 1

      if (i + 1 < end) {
        val q1 = bytes(i + 1) & 0xff
        result(ri) = ((q0 >> 5) & 0x07) | ((q1 & 0x03) << 3); ri += 1
        result(ri) = (q1 >> 2) & 0x1f; ri += 1

        if (i + 2 < end) {
          val q2 = bytes(i + 2) & 0xff
          result(ri) = ((q1 >> 7) & 0x01) | ((q2 & 0x0f) << 1); ri += 1

          if (i + 3 < end) {
            val q3 = bytes(i + 3) & 0xff
            result(ri) = ((q2 >> 4) & 0x0f) | ((q3 & 0x01) << 4); ri += 1
            result(ri) = (q3 >> 1) & 0x1f; ri += 1

            if (i + 4 < end) {
              val q4 = bytes(i + 4) & 0xff
              result(ri) = ((q3 >> 6) & 0x03) | ((q4 & 0x07) << 2); ri += 1
              result(ri) = (q4 >> 3) & 0x1f; ri += 1
            }
          }
        }
      }
      i += 5
    }

    if (ri < result.length) java.util.Arrays.copyOf(result, ri) else result
  }
}

// SimHash.scala
//package chromaprint

object SimHash {

  val length: Int = 32

  // mvtiaine: modified to support generating multiple/longer hashes as BigInt,
  // instead of single UInt from small part of the data
  // also IndexedSeq[UInt] to Array[Int] to reduce overhead and optimized loop
  def apply(data: Array[Int], hashes: Int = 1): BigInt = {
    if (data.isEmpty) {
      return BigInt(0)
    }
    val groupSize = (data.length + hashes - 1) / hashes
    val groups = if (groupSize > 0) data.grouped(groupSize).toSeq else Seq(data)

    groups.map { group =>
      val counts = new Array[Int](length)
      for (el <- group) {
        var i = 0
        while (i < length) {
          if ((el & (1 << i)) == 0) counts(i) -= 1 else counts(i) += 1
          i += 1
        }
      }
      var result = 0
      var i = 0
      while (i < length) {
        if (counts(i) > 0) result |= (1 << i)
        i += 1
      }
      result
    }.foldLeft(BigInt(0))((acc, hash) => (acc << 32) | BigInt(hash.toLong & 0xFFFFFFFFL))
  }
}
