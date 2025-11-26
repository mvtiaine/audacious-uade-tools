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
  player: String,
  subsong: Int,
  normalizedSubsong: Int, // normalized to start from 1
  audioBytes: Int,
  audioMd5: String,
  audioChromaprint: String,
  audioSimHash: String,
  audioHash: String,
  audioTag: String
)

def parseAudioTsv(tsv: String, withSimHash: Boolean) = {
  var prevMd5 = ""
  var prevPlayer = ""
  var fixsubsong = false
  Using(scala.io.Source.fromFile(tsv)(using scala.io.Codec.ISO8859))(_.getLines.toSeq.flatMap(line => {
    val l = line.split("\t")
    val md5 = l(0).take(12)
    val player = l(1)
    val subsong = l(2).toInt
    val audioBytes = l(3).toInt
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
    if (audioBytes > 0) {
      val audioMd5 = if (l.length >= 5) l(4) else ""
      val audioChromaprint = if (l.length >= 6) l(5) else ""
      // require at least 12s of audio for simhash comparison to minimize false positives
      val (audioSimHash, simTag) = if (withSimHash && audioChromaprint.nonEmpty && audioBytes > 2 * 11025 * 12) {
        val (algo,data) = decodeChromaprint(audioChromaprint) : @unchecked
        val numHashes = Math.max(1, audioBytes / (2 * 11025 * 3)) // one hash per 3s of audio
        val h = SimHash(data, numHashes)
        val hex = h.toString(16)
        (hex, h.bitLength/4)
      } else ("","")
      val audioHash = Seq(audioSimHash, audioChromaprint, audioMd5).filter(_.nonEmpty).head
      val audioTag = normalizedSubsong + "-" + player + "-" + (if (audioHash == audioSimHash) simTag else audioHash)
      //System.err.println(s"AUDIOTAG: ${md5}: ${audioTag}")
      Some(AudioFingerprint(
        md5,
        player,
        subsong,
        normalizedSubsong,
        audioBytes,
        audioMd5,
        audioChromaprint,
        audioSimHash,
        audioHash,
        audioTag,
      ))
    } else None
  }).distinct.toBuffer).get
}

lazy val audioFingerprints =
  Paths.get("sources/audio").toFile.listFiles.filter(_.getName.endsWith(".tsv")).par.flatMap(tsv =>
    parseAudioTsv(tsv.getAbsolutePath, withSimHash = true)
  ).seq.distinct.toBuffer

lazy val audioByPlayerAndMd5 = audioFingerprints.groupBy(e => (e.player, e.md5))
  .par.mapValues(_.sortBy(_.normalizedSubsong).distinct)

lazy val audioByAudioTags = audioFingerprints.groupBy(e =>
  audioByPlayerAndMd5((e.player, e.md5)).map(_.audioTag).sorted.distinct).par.mapValues(_.distinct).seq
