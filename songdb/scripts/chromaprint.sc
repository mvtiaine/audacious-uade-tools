// SPDX-License-Identifier: MIT
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>
// see below for further copyrights

//> using dep org.scodec::scodec-bits::1.2.4
//> using dep org.typelevel::spire::0.18.0

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala
import scala.util.boundary, boundary.break
import spire.math._

val fpCache = new ConcurrentHashMap[String, (Int, IndexedSeq[UInt])]().asScala

def decodeChromaprint(chromaprint: String): (Int, IndexedSeq[UInt]) = {
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
  val Seq(fp1, fp2) = Seq(chromaprint1, chromaprint2).sorted
  similarityCache.getOrElseUpdate((fp1, fp2, minSimilarity), {
    val (algo0, data0) = decodeChromaprint(fp1)
    val (algo, data) = decodeChromaprint(fp2)
    assert(algo0 == algo)
    chromaSimilarityFast(algo0, data0, algo, data, minSimilarity)
  })
}

def chromaSimilarityFast(
  algo1: Int,
  data1: IndexedSeq[spire.math.UInt],
  algo2: Int,
  data2: IndexedSeq[spire.math.UInt],
  threshold: Double,
  fuzziness: Int = 3
): Double = {
  if (algo1 != algo2) {
    return 0.0
  }

  var maxSimilarity = 0.0
  val offsets = Seq(0) ++ (1 to fuzziness).flatMap(i => Seq(-i, i))

  boundary {
    for (offset <- offsets) {
      var totalScore = 0
      var overlap = 0
      var i = 0
    
      while (i < data1.length) {
        val j = i + offset
        if (j >= 0 && j < data2.length) {
          val xorValue = data1(i) ^ data2(j)
          totalScore += (32 - Integer.bitCount(xorValue.toInt))
          overlap += 1
        }
        i += 1
      }
    
      if (overlap > 0) {
        val similarity = totalScore.toDouble / (overlap * 32.0)
        if (similarity > maxSimilarity) {
          maxSimilarity = similarity
          if (maxSimilarity >= threshold) break()
        }
      }
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

// mvtiaine: vibe optimized with Claude Sonnet 4

import spire.math.{UInt, UShort}

object FingerprintDecompressor {

  final class DecompressorException(message: String) extends Exception(message)

  def apply(data: String): Either[DecompressorException,(Int, IndexedSeq[UInt])] =
    Base64.decode(data) match {
      case Right(bytes) =>
        apply(bytes)
      case Left(e) =>
        Left(new DecompressorException("Invalid Base64 string: " + e.getMessage))
    }

  def apply(bytes: IndexedSeq[Byte]): Either[DecompressorException,(Int, IndexedSeq[UInt])] =
    if (bytes.length < 5) {
      Left(new DecompressorException("Invalid fingerprint (shorter than 5 bytes)"))
    } else {
      val ( header, body ) = bytes.splitAt(4)
      val algorithm: Int = header(0).toInt
      val length: Int = ((0xff & header(1)) << 16) | ((0xff & header(2)) << 8) | (0xff & header(3))
      if (algorithm < 0) {
        Left(new DecompressorException("Invalid algorithm"))
      } else if (length < 1) {
        Left(new DecompressorException("Invalid length"))
      } else {
        extractNormalBits(
          body,
          length
        ) match {
          case Left(e) =>
            Left(e)
          case Right(normalBits) =>
            extractExceptionBits(
              body,
              normalBits
            ) match {
              case Left(e) =>
                Left(e)
              case Right(exceptionBits) =>
                Right(
                  (
                    algorithm,
                    unpackBits(combineBits(normalBits, exceptionBits))
                  )
                )
            }
        }
      }
    }

  private def extractNormalBits
  (
    bytes: IndexedSeq[Byte],
    length: Int
  ): Either[DecompressorException,IndexedSeq[IndexedSeq[UShort]]] = {

    // Pre-allocate result array with known size
    val result = Array.ofDim[Vector[UShort]](length)
    var resultIndex = 0
    var current = Vector.empty[UShort]
    var offset = 0

    val triplets = bytesToTriplets(bytes)
    val tripletsLength = triplets.length

    while (resultIndex < length && offset < tripletsLength) {
      val bit = triplets(offset)
      current = current :+ bit
      offset += 1
      
      if (bit.toInt == 0) {
        result(resultIndex) = current
        resultIndex += 1
        current = Vector.empty
      }
    }

    if (resultIndex < length) {
      Left(new DecompressorException("Not enough normal bits"))
    } else {
      Right(result.toIndexedSeq)
    }
  }

  // Optimized bit manipulation with lookup tables for common operations
  private val mask3Bits = Array.tabulate(256)(i => i & 0x07)
  private val mask5Bits = Array.tabulate(256)(i => i & 0x1f)
  
  private def bytesToTriplets(bytes: IndexedSeq[Byte]): IndexedSeq[UShort] = {
    val result = Vector.newBuilder[UShort]
    result.sizeHint(bytes.length * 8 / 3) // Rough estimate for better performance
    
    var i = 0
    val bytesLength = bytes.length
    
    while (i < bytesLength) {
      val b0 = bytes(i) & 0xff
      result += UShort(mask3Bits(b0))
      result += UShort((b0 & 0x38) >> 3)
      
      if (i + 1 < bytesLength) {
        val b1 = bytes(i + 1) & 0xff
        result += UShort(((b0 & 0xc0) >> 6) | ((b1 & 0x01) << 2))
        result += UShort((b1 & 0x0e) >> 1)
        result += UShort((b1 & 0x70) >> 4)
        
        if (i + 2 < bytesLength) {
          val b2 = bytes(i + 2) & 0xff
          result += UShort(((b1 & 0x80) >> 7) | ((b2 & 0x03) << 1))
          result += UShort((b2 & 0x1c) >> 2)
          result += UShort((b2 & 0xe0) >> 5)
        }
      }
      i += 3
    }
    
    result.result()
  }

  private def packedTripletSize(size: Int): Int =
    (size * 3 + 7) >> 3  // Bit shift instead of division

  private def extractExceptionBits
  (
    body: IndexedSeq[Byte],
    normalBits: IndexedSeq[IndexedSeq[UShort]]
  ): Either[DecompressorException,IndexedSeq[IndexedSeq[Option[UShort]]]] = {

    val normalBitsLength = normalBits.iterator.map(_.length).sum
    val quintets = bytesToQuintets(body.drop(packedTripletSize(normalBitsLength)))

    // Pre-compute exception positions more efficiently
    val exceptionPositions = Array.ofDim[Array[Int]](normalBits.length)
    var totalExceptions = 0
    
    var i = 0
    while (i < normalBits.length) {
      val positions = normalBits(i).zipWithIndex.collect {
        case (bit, idx) if bit.toInt == 7 => idx
      }.toArray
      exceptionPositions(i) = positions
      totalExceptions += positions.length
      i += 1
    }

    if (totalExceptions > quintets.length) {
      Left(new DecompressorException("Not enough exception bits"))
    } else {
      var quintetOffset = 0
      val result = Array.ofDim[IndexedSeq[Option[UShort]]](normalBits.length)
      
      i = 0
      while (i < normalBits.length) {
        val normalBitLength = normalBits(i).length
        val exceptions = Array.fill[Option[UShort]](normalBitLength)(None)
        
        val positions = exceptionPositions(i)
        var j = 0
        while (j < positions.length) {
          exceptions(positions(j)) = Some(quintets(quintetOffset))
          quintetOffset += 1
          j += 1
        }
        
        result(i) = exceptions.toIndexedSeq
        i += 1
      }
      
      Right(result.toIndexedSeq)
    }
  }

  private def bytesToQuintets(bytes: IndexedSeq[Byte]): IndexedSeq[UShort] = {
    val result = Vector.newBuilder[UShort]
    result.sizeHint(bytes.length * 8 / 5) // Rough estimate
    
    var i = 0
    val bytesLength = bytes.length
    
    while (i < bytesLength) {
      val q0 = bytes(i) & 0xff
      result += UShort(mask5Bits(q0))
      
      if (i + 1 < bytesLength) {
        val q1 = bytes(i + 1) & 0xff
        result += UShort(((q0 & 0xe0) >> 5) | ((q1 & 0x03) << 3))
        result += UShort((q1 & 0x7c) >> 2)
        
        if (i + 2 < bytesLength) {
          val q2 = bytes(i + 2) & 0xff
          result += UShort(((q1 & 0x80) >> 7) | ((q2 & 0x0f) << 1))
          
          if (i + 3 < bytesLength) {
            val q3 = bytes(i + 3) & 0xff
            result += UShort(((q2 & 0xf0) >> 4) | ((q3 & 0x01) << 4))
            result += UShort((q3 & 0x3e) >> 1)
            
            if (i + 4 < bytesLength) {
              val q4 = bytes(i + 4) & 0xff
              result += UShort(((q3 & 0xc0) >> 6) | ((q4 & 0x07) << 2))
              result += UShort((q4 & 0xf8) >> 3)
            }
          }
        }
      }
      i += 5
    }
    
    result.result()
  }

  private def combineBits
  (
    normalBits: IndexedSeq[IndexedSeq[UShort]],
    exceptionBits: IndexedSeq[IndexedSeq[Option[UShort]]]
  ): IndexedSeq[UShort] = {
    val result = Vector.newBuilder[UShort]
    result.sizeHint(normalBits.iterator.map(_.length).sum)
    
    var i = 0
    while (i < normalBits.length) {
      val normal = normalBits(i)
      val exceptions = exceptionBits(i)
      
      var j = 0
      while (j < normal.length) {
        val normalBit = normal(j)
        if (normalBit.toInt == 7) {
          exceptions(j) match {
            case Some(exceptionBit) => result += normalBit + exceptionBit
            case None => throw new RuntimeException("Exception bit not found")
          }
        } else {
          result += normalBit
        }
        j += 1
      }
      i += 1
    }
    
    result.result()
  }

  private def unpackBits(bits: IndexedSeq[UShort]): IndexedSeq[UInt] = {
    val result = Vector.newBuilder[UInt]
    result.sizeHint(bits.length / 2) // Rough estimate
    
    var value = UInt(0)
    var lastBit = UInt(0)
    var previousValue = UInt(0)
    
    var i = 0
    while (i < bits.length) {
      val bit = bits(i)
      if (bit.toInt == 0) {
        val finalValue = if (result.knownSize == 0) value else value ^ previousValue
        result += finalValue
        previousValue = finalValue
        value = UInt(0)
        lastBit = UInt(0)
      } else {
        val nextLast = lastBit + UInt(bit.toInt)
        value = value | (UInt(1) << (nextLast.toInt - 1))
        lastBit = nextLast
      }
      i += 1
    }
    
    result.result()
  }
}

// SimHash.scala
//package chromaprint

import spire.math.UInt

object SimHash {

  val length: Int = 32

  // mvtiaine: modified to support generating multiple/longer hashes as BigInt,
  // instead of single UInt from small part of the data
  def apply(data: Seq[UInt], hashes: Int = 1): BigInt = {
    if (data.isEmpty) {
      return BigInt(0)
    }
    val groupSize = (data.length + hashes - 1) / hashes
    val groups = if (groupSize > 0) data.grouped(groupSize).toSeq else Seq(data)

    groups.map { group =>
      group.foldLeft(Vector.fill[Int](length)(0)) {
        (hash, el) =>
          (0 until length).foldLeft(hash) {
            (iHash, i) =>
              iHash.updated(
                i,
                iHash(i) + (
                  if ((el & (UInt(1) << i)).toInt == 0) {
                    -1
                  } else {
                    1
                  })
              )
          }
      }.zipWithIndex.foldLeft(UInt(0)){
        (result, next) =>
          if (next._1 > 0) {
            result | (UInt(1) << next._2)
          } else {
            result
          }
      }
    }.foldLeft(BigInt(0))((acc, hash) => (acc << 32) | BigInt(hash.toLong & 0xFFFFFFFFL))
  }
}
