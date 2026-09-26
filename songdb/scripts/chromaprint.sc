// SPDX-License-Identifier: GPL-2.0-or-later AND MIT AND CC-PDM-1.0
// SPDX-AI-Disclosure: ai-assisted
// Copyright (C) 2025-2026 Matti Tiainen <mvtiaine@cc.hut.fi>
// see below for further copyrights

//> using dep org.lz4:lz4-java:1.8.1

import java.util.Arrays
import java.util.Base64
import java.util.concurrent.ConcurrentHashMap
import net.jpountz.xxhash.XXHashFactory

// number of 32-bit words per silence/content scan window
val chunkSize = 64

def isSilentChunk(data: Array[Int], len: Int, offset: Int = 0): Boolean = {
  if (len <= 0) return true
  val zeroTrue  = (9 * len) / 10          // zeroCount  >  zeroTrue  <=> zeroRatio   >  0.9
  val bitsDead1 = (32 * len + 199) / 200 // totalBits >= bitsDead1 <=> setBitRatio  >= 0.005
  val bitsDead3 = (32 * len + 49) / 50   // totalBits >= bitsDead3 <=> setBitRatio  >= 0.02
  val uniqDead  = (len + 19) / 20        // uniq      >= uniqDead  <=> repetitionRatio >= 0.05
  var totalBits = 0
  var zeroCount = 0
  // the first four distinct values seen; uniq saturates at 4 == ">= 4"
  var u0 = 0
  var u1 = 0
  var u2 = 0
  var uniq = 0
  var i = 0
  while (i < len) {
    val v = data(offset + i)
    totalBits += Integer.bitCount(v)
    if (v == 0) zeroCount += 1
    if (uniq == 0) { u0 = v; uniq = 1 }
    else if (uniq == 1) { if (v != u0) { u1 = v; uniq = 2 } }
    else if (uniq == 2) { if (v != u0 && v != u1) { u2 = v; uniq = 3 } }
    else if (uniq == 3) { if (v != u0 && v != u1 && v != u2) uniq = 4 }
    if (zeroCount > zeroTrue) return true
    if (totalBits >= bitsDead1 && uniq >= 3 && (totalBits >= bitsDead3 || uniq >= uniqDead)
        && zeroCount + len - i - 1 <= zeroTrue) return false
    i += 1
  }
  if (totalBits < bitsDead1) return true
  if (uniq <= 2) return true
  if (totalBits < bitsDead3 && uniq < uniqDead) return true
  false
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

final class FP (val algo: Int, val data: Array[Int], val key: Long = 0L) {
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
  override lazy val hashCode: Int = algo * 31 + Arrays.hashCode(data)
  override def equals(obj: Any): Boolean = obj match {
    case other: FP => algo == other.algo && length == other.length && Arrays.equals(data, other.data)
    case _ => false
  }
}
val fpCache = new ConcurrentHashMap[Long, FP](500_000)

// xxhash64 of the base64 decoded chromaprint bytes, used as a compact cache key
// so the full base64 chromaprint string does not need to be retained in memory
val xxh64 = XXHashFactory.fastestInstance().hash64()
val xxh64Seed = 0

def chromaprintHash(bytes: Array[Byte]): Long =
  xxh64.hash(bytes, 0, bytes.length, xxh64Seed)

def chromaprintHash(chromaprint: String): Long =
  chromaprintHash(Base64.getUrlDecoder.decode(chromaprint))

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

// similarity between two already-decoded FPs.
def chromaSimilarityFPs(fp1: FP, fp2: FP): Double = {
  assert(fp1.algo == fp2.algo)
  val (s1, e1) = fp1.contentBounds
  val (s2, e2) = fp2.contentBounds
  chromaSimilarityFast(fp1.algo, s1, e1, fp1.data, fp2.algo, s2, e2, fp2.data)
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

// Chromaprint decoding and SimHash code, with some modifications, originally from:
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

// mvtiaine: note that the code has been vibe optimized with various LLM models, so the current code differs quite a bit from the original implementation
*/

// FingerprintDecompressor.scala
//package chromaprint

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
      val v = triplets(tripletsUsed).toInt
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
      val v = triplets(ti).toInt
      if (v == 0) {
        val finalValue = if (resultIdx == 0) value else value ^ previousValue
        result(resultIdx) = finalValue
        previousValue = finalValue
        resultIdx += 1
        value = 0
        lastBit = 0
      } else {
        val actual = if (v == 7) { val q = quintets(quintetIdx).toInt; quintetIdx += 1; v + q } else v
        lastBit += actual
        value |= (1 << (lastBit - 1))
      }
      ti += 1
    }

    Right((algorithm, result))
  }

  private def packedTripletSize(size: Int): Int =
    (size * 3 + 7) >> 3

  private def bytesToTriplets(bytes: Array[Byte], start: Int, end: Int): Array[Byte] = {
    val result = new Array[Byte]((end - start) * 8 / 3)
    var ri = 0
    var i = start

    // every stored value is <= 0x1f, so the narrowing is lossless and the read-back needs no mask
    while (i < end) {
      val b0 = bytes(i) & 0xff
      result(ri) = (b0 & 0x07).toByte; ri += 1
      result(ri) = ((b0 >> 3) & 0x07).toByte; ri += 1

      if (i + 1 < end) {
        val b1 = bytes(i + 1) & 0xff
        result(ri) = ((((b0 >> 6) & 0x03) | ((b1 & 0x01) << 2))).toByte; ri += 1
        result(ri) = ((b1 >> 1) & 0x07).toByte; ri += 1
        result(ri) = ((b1 >> 4) & 0x07).toByte; ri += 1

        if (i + 2 < end) {
          val b2 = bytes(i + 2) & 0xff
          result(ri) = ((((b1 >> 7) & 0x01) | ((b2 & 0x03) << 1))).toByte; ri += 1
          result(ri) = ((b2 >> 2) & 0x07).toByte; ri += 1
          result(ri) = ((b2 >> 5) & 0x07).toByte; ri += 1
        }
      }
      i += 3
    }

    result
  }

  private def bytesToQuintets(bytes: Array[Byte], start: Int, end: Int): Array[Byte] = {
    val result = new Array[Byte]((end - start) * 8 / 5)
    var ri = 0
    var i = start

    // every stored value is <= 0x1f, so the narrowing is lossless (see bytesToTriplets)
    while (i < end) {
      val q0 = bytes(i) & 0xff
      result(ri) = (q0 & 0x1f).toByte; ri += 1

      if (i + 1 < end) {
        val q1 = bytes(i + 1) & 0xff
        result(ri) = ((((q0 >> 5) & 0x07) | ((q1 & 0x03) << 3))).toByte; ri += 1
        result(ri) = ((q1 >> 2) & 0x1f).toByte; ri += 1

        if (i + 2 < end) {
          val q2 = bytes(i + 2) & 0xff
          result(ri) = ((((q1 >> 7) & 0x01) | ((q2 & 0x0f) << 1))).toByte; ri += 1

          if (i + 3 < end) {
            val q3 = bytes(i + 3) & 0xff
            result(ri) = ((((q2 >> 4) & 0x0f) | ((q3 & 0x01) << 4))).toByte; ri += 1
            result(ri) = ((q3 >> 1) & 0x1f).toByte; ri += 1

            if (i + 4 < end) {
              val q4 = bytes(i + 4) & 0xff
              result(ri) = ((((q3 >> 6) & 0x03) | ((q4 & 0x07) << 2))).toByte; ri += 1
              result(ri) = ((q4 >> 3) & 0x1f).toByte; ri += 1
            }
          }
        }
      }
      i += 5
    }

    result
  }
}

// SimHash.scala
//package chromaprint

object SimHash {

  val length: Int = 32

  def apply(data: Array[Int], hashes: Int = 1): BigInt = {
    val n = data.length
    if (n == 0) return BigInt(0)
    val nh = math.max(1, hashes)
    val groupSize = math.max(1, (n + nh - 1) / nh)
    val groups = (n + groupSize - 1) / groupSize
    // bits needed to count up to groupSize (the last group may be shorter, so this is an upper bound)
    val b = 32 - Integer.numberOfLeadingZeros(groupSize)
    val planes = new Array[Int](b)
    val mag = new Array[Byte](groups * 4)
    var g = 0
    while (g < groups) {
      val start = g * groupSize
      val end = math.min(start + groupSize, n)
      val k = end - start
      Arrays.fill(planes, 0)
      var i = start
      while (i < end) {
        var carry = data(i)
        var j = 0
        while (carry != 0 && j < b) {
          val p = planes(j)
          planes(j) = p ^ carry
          carry = p & carry
          j += 1
        }
        i += 1
      }
      // bit set iff count > k/2 (counts = 2*pop - k, so counts > 0 <=> pop > floor(k/2))
      val t = k / 2
      var gt = 0
      var eq = -1
      var j = b - 1
      while (j >= 0) {
        val cj = planes(j)
        val tj = if (((t >>> j) & 1) != 0) -1 else 0
        gt |= eq & (cj & ~tj)
        eq &= ~(cj ^ tj)
        j -= 1
      }
      val o = g * 4
      mag(o) = (gt >>> 24).toByte
      mag(o + 1) = (gt >>> 16).toByte
      mag(o + 2) = (gt >>> 8).toByte
      mag(o + 3) = gt.toByte
      g += 1
    }
    BigInt(new java.math.BigInteger(1, mag))
  }
}
