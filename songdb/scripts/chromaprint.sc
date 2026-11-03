// SPDX-License-Identifier: MIT
// SPDX-AI-Disclosure: ai-assisted

// Chromaprint decoding and SimHash code, with lots of modifications, originally from:
// https://github.com/mgdigital/Chromaprint.scala
// Note that the code has been vibe optimized with various LLM models,
// so the current code differs quite a bit from the original implementation

// Original copyright notice:
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
import java.util.Arrays
import java.util.Base64

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
