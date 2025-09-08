// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import java.nio.file.Paths
import scala.util.Using
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._

import chromaprint._
import songlengths._

case class AudioFingerprint (
  md5: String,
  subsong: Int,
  normalizedSubsong: Int, // normalized to start from 1
  audioBytes: Int,
  audioMd5: String,
  audioChromaprint: String,
  audioSimHash: String,
  audioHash: String,
  tag: String
)

lazy val audioFingerprints = {
  Paths.get("sources/audio").toFile.listFiles.filter(_.getName.endsWith(".tsv")).par.flatMap(tsv => {
    var prevMd5 = ""
    var fixsubsong = false
    Using(scala.io.Source.fromFile(tsv)(using scala.io.Codec.ISO8859))(_.getLines.toSeq.flatMap(line => {
      val l = line.split("\t")
      val audioBytes = l(2).toInt
      val md5 = l(0).take(12)
      val subsong = l(1).toInt
      var normalizedSubsong = subsong
      if (md5 != prevMd5) {
        fixsubsong = false
        prevMd5 = md5
      }
      if (subsong == 0) {
        fixsubsong = true
      }
      if (fixsubsong) {
        normalizedSubsong += 1
      }
      if (audioBytes > 0) {
        val audioMd5 = if (l.length >= 4) l(3) else ""
        val audioChromaprint = if (l.length >= 5) l(4) else ""
        // require at least 10s of audio for simhash comparison to minimize false positives
        val (audioSimHash, simTag) = if (audioChromaprint.nonEmpty && audioBytes > 2 * 11025 * 10) {
          val (algo,data) = decodeChromaprint(audioChromaprint) : @unchecked
          val numHashes = Math.max(1, audioBytes / (2 * 11025 * 3)) // one hash per 3s of audio
          val h = SimHash(data, numHashes)
          val hex = h.toString(16)
          val player = songlengthsByMd5(md5).head.player
          (hex, player + "-" + h.bitLength/4)
        } else ("","")
        val audioHash = Seq(audioSimHash, audioChromaprint, audioMd5).filter(_.nonEmpty).head
        //System.err.println(s"AUDIOTAG: ${md5 + ": " + subsong + "-" + (if (audioHash == audioSimHash) simTag else audioHash)}")
        Some(AudioFingerprint(
          md5,
          subsong,
          normalizedSubsong,
          audioBytes,
          audioMd5,
          audioChromaprint,
          audioSimHash,
          audioHash,
          normalizedSubsong + "-" + (if (audioHash == audioSimHash) simTag else audioHash),
        ))
      } else None
    }).distinct.toBuffer).get
  })
}.seq.distinct.toBuffer

lazy val audioByMd5 = audioFingerprints.groupBy(_.md5)
  .par.mapValues(_.sortBy(_.normalizedSubsong).distinct)

lazy val audioByAudioTags = audioFingerprints.groupBy(e =>
  audioByMd5(e.md5).map(_.tag).sorted.distinct).par.mapValues(_.distinct).seq
