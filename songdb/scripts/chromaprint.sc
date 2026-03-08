// SPDX-License-Identifier: MIT
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>
// see below for further copyrights

//> using dep org.scodec::scodec-bits::1.2.4

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala
import scala.util.boundary, boundary.break

val fpCache = new ConcurrentHashMap[String, (Int, Array[Int])]().asScala

def decodeChromaprint(chromaprint: String): (Int, Array[Int]) = {
  fpCache.getOrElseUpdate(chromaprint, {
    val Right(algo, data) = FingerprintDecompressor(chromaprint) : @unchecked
    (algo, data)
  })
}

val similarityCache = new ConcurrentHashMap[(String, String, Double), Double]().asScala

def chromaSimilarity(chromaprint1: String, chromaprint2: String, minSimilarity: Double): Double = {
  if (chromaprint1 == chromaprint2) {
    return 1.0
  }
  val (fp1, fp2) = if (chromaprint1 < chromaprint2) (chromaprint1, chromaprint2) else (chromaprint2, chromaprint1)
  similarityCache.getOrElseUpdate((fp1, fp2, minSimilarity), {
    val (algo0, data0) = decodeChromaprint(fp1)
    val (algo, data) = decodeChromaprint(fp2)
    assert(algo0 == algo)
    chromaSimilarityFast(algo0, data0, algo, data, minSimilarity)
  })
}

def chromaSimilarityFast(
  algo1: Int,
  data1: Array[Int],
  algo2: Int,
  data2: Array[Int],
  threshold: Double,
  fuzziness: Int = 3
): Double = {
  if (algo1 != algo2) {
    return 0.0
  }

  val len1 = data1.length
  val len2 = data2.length
  var maxSimilarity = 0.0

  boundary {
    var oi = 0
    while (oi <= fuzziness) {
      var pass = 0
      while (pass < (if (oi == 0) 1 else 2)) {
        val offset = if (pass == 0) -oi else oi
        val iStart = Math.max(0, -offset)
        val iEnd = Math.min(len1, len2 - offset)
        var totalScore = 0
        var overlap = 0
        var i = iStart

        while (i < iEnd) {
          totalScore += (32 - Integer.bitCount(data1(i) ^ data2(i + offset)))
          overlap += 1
          i += 1
        }

        if (overlap > 0) {
          val similarity = totalScore.toDouble / (overlap * 32.0)
          if (similarity > maxSimilarity) {
            maxSimilarity = similarity
            if (maxSimilarity >= threshold) break()
          }
        }
        pass += 1
      }
      oi += 1
    }
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

// Base64.scala
//package chromaprint

import scodec.bits.{Bases, ByteVector}

object Base64 {

  final class EncoderException(message: String) extends Exception(message)

  val alphabet: Bases.Base64Alphabet = Bases.Alphabets.Base64Url

  def apply(data: Seq[Byte]): String =
    apply(ByteVector(data))

  def apply(data: ByteVector): String =
    data.toBase64(alphabet).
      replaceAll("=+$", "")

  def unapply(str: String): Option[IndexedSeq[Byte]] =
    decode(str) match {
      case Left(_) =>
        None
      case Right(bytes) =>
        Some(bytes)
    }

  def decode(str: String): Either[EncoderException,IndexedSeq[Byte]] =
    ByteVector.fromBase64Descriptive(str, alphabet) match {
      case Right(bv) =>
        Right(bv.toArray.toIndexedSeq)
      case Left(err) =>
        Left(new EncoderException(err))
    }

}

// FingerprintDecompressor.scala
//package chromaprint

// mvtiaine: vibe optimized with Claude Sonnet 4 / Claude Opus 4

object FingerprintDecompressor {

  final class DecompressorException(message: String) extends Exception(message)

  def apply(data: String): Either[DecompressorException,(Int, Array[Int])] =
    Base64.decode(data) match {
      case Right(bytes) =>
        apply(bytes.toArray)
      case Left(e) =>
        Left(new DecompressorException("Invalid Base64 string: " + e.getMessage))
    }

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
