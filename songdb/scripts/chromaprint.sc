// SPDX-License-Identifier: MIT

//> using dep org.scodec::scodec-bits::1.2.1
//> using dep org.typelevel::spire::0.18.0

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

    var result: Vector[Vector[UShort]] = Vector.empty
    var current: Vector[UShort] = Vector.empty

    // mvtiaine: offset tracking optimization
    //def offset: Int = result.flatten.length + current.length
    var offset: Int = 0
    def incomplete: Boolean = result.length < length

    val triplets = bytesToTriplets(bytes)

    while (incomplete && triplets.lift(offset).isDefined) {
      val bit = triplets(offset)
      current = current :+ bit
      offset += 1 // mvtiaine: offset tracking optimization
      if (bit.toInt == 0) {
        result = result :+ current
        current = Vector.empty
      }
    }

    if (incomplete) {
      Left(new DecompressorException("Not enough normal bits"))
    } else {
      Right(result)
    }
  }

  private def bytesToTriplets(bytes: IndexedSeq[Byte]): IndexedSeq[UShort] =
    bytes.grouped(3).flatMap { t =>
      Vector(
        t(0) & 0x07,
        (t(0) & 0x38) >> 3
      ) ++ (t.lift(1) match {
        case None =>
          Vector.empty[Int]
        case Some(t1) =>
          Vector(
            ((t(0) & 0xc0) >> 6) | ((t1 & 0x01) << 2),
            (t1 & 0x0e) >> 1,
            (t1 & 0x70) >> 4
          ) ++ (t.lift(2) match {
            case None =>
              Vector.empty[Int]
            case Some(t2) =>
              Vector(
                ((t1 & 0x80) >> 7) | ((t2 & 0x03) << 1),
                (t2 & 0x1c) >> 2,
                (t2 & 0xe0) >> 5
              )
          })
      })
    }.map(UShort(_)).toVector

  private def packedTripletSize(size: Int): Int =
    (size * 3 + 7) / 8

  private def extractExceptionBits
  (
    body: IndexedSeq[Byte],
    normalBits: IndexedSeq[IndexedSeq[UShort]]
  ): Either[DecompressorException,IndexedSeq[IndexedSeq[Option[UShort]]]] = {

    val quintets = bytesToQuintets(body.drop(packedTripletSize(normalBits.flatten.length)))

    val maxNormals: Map[Int,Set[Int]] = normalBits.zipWithIndex.
      map(t => (t._2, t._1.zipWithIndex.filter(_._1.toInt == 7).map(_._2).toSet)).
      foldLeft(Map.empty[Int,Set[Int]]){ (m, n) => m.updated(n._1, n._2)}

    val requiredExceptionBits = maxNormals.values.flatten.toVector.length

    if (requiredExceptionBits > quintets.length) {

      Left(new DecompressorException("Not enough exception bits"))
    } else {

      var offset: Int = 0

      Right(normalBits.indices.map{ i =>
        maxNormals(i).foldLeft(Vector.fill[Option[UShort]](normalBits(i).length)(None)){ (v, n) =>
          val b = quintets(offset)
          offset += 1
          v.updated(n, Some(b))
        }
      })
    }
  }

  // scalastyle:off magic.number

  private def bytesToQuintets(bytes: IndexedSeq[Byte]): IndexedSeq[UShort] =
    bytes.grouped(5).flatMap{ q =>
      Vector(
        q(0) & 0x1f
      ) ++ (q.lift(1) match {
        case None =>
          Vector.empty[Int]
        case Some(q1) =>
          Vector(
            ((q(0) & 0xe0) >> 5) | ((q1 & 0x03) << 3),
            (q1 & 0x7c) >> 2
          ) ++ (q.lift(2) match {
            case None =>
              Vector.empty[Int]
            case Some(q2) =>
              Vector(
                ((q1 & 0x80) >> 7) | ((q2 & 0x0f) << 1)
              ) ++ (q.lift(3) match {
                case None =>
                  Vector.empty[Int]
                case Some(q3) =>
                  Vector(
                    ((q2 & 0xf0) >> 4) | ((q3 & 0x01) << 4),
                    (q3 & 0x3e) >> 1
                  ) ++ (q.lift(4) match {
                    case None =>
                      Vector.empty[Int]
                    case Some(q4) =>
                      Vector(
                        ((q3 & 0xc0) >> 6) | ((q4 & 0x07) << 2),
                        (q4 & 0xf8) >> 3
                      )
                  })
              })
          })
      })
    }.map(UShort(_)).toIndexedSeq

  // scalastyle:on magic.number

  private def combineBits
  (
    normalBits: IndexedSeq[IndexedSeq[UShort]],
    exceptionBits: IndexedSeq[IndexedSeq[Option[UShort]]]
  ): IndexedSeq[UShort] =
    normalBits.zip(exceptionBits).flatMap{ p =>
      p._1.zipWithIndex.map{ b =>
        if (b._1.toInt == 7) {
          b._1 + p._2(b._2).getOrElse{throw new RuntimeException("Exception bit not found")}
        } else {
          b._1
        }
      }
    }

  private def unpackBits(bits: IndexedSeq[UShort]): IndexedSeq[UInt] =
    bits.foldLeft((UInt(0), UInt(0), Vector.empty[UInt])){ (t, bit) =>
      val ( value, lastBit, result ) = t
      if (bit.toInt == 0) {
        (
          UInt(0),
          UInt(0),
          result :+ result.lastOption.map(value ^ _).getOrElse(value)
        )
      } else {
        val nextLast = lastBit + UInt(bit.toInt)
        (
          value | UInt(1) << (nextLast.toInt - 1),
          nextLast,
          result
        )
      }
    }._3

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
