// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import java.nio.file.Paths
import scala.util.Using
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._

import chromaprint._
import songlengths._

final case class AudioFingerprint (
  md5: String,
  player: String,
  subsong: Int,
  normalizedSubsong: Int, // normalized to start from 1
  audioBytes: Int,
  audioMd5: String,
  audioChromaprint: String,
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
      // require at least 9s of audio for simhash comparison to minimize false positives
      val (audioSimHash, simTags) = if (withSimHash && audioChromaprint.nonEmpty && audioBytes > 2 * 11025 * 9) {
        val fp = decodeChromaprint(audioChromaprint) : @unchecked
        val numHashes = Math.max(1, audioBytes / (2 * 11025 * 3)) // one hash per 3s of audio
        val h = SimHash(fp.data, numHashes)
        val hex = h.toString(16)
        (hex, Seq((h.bitLength+1)/4, (h.bitLength-1)/4).distinct)
      } else ("",Seq.empty[Int])
      val audioHash = Seq(audioSimHash, audioChromaprint, audioMd5).filter(_.nonEmpty).head
      val audioTags = if (audioHash == audioSimHash) simTags.map(t => normalizedSubsong + "-" + player + "-" + t) else Seq(normalizedSubsong + "-" + player + "-" + audioHash)
      //System.err.println(s"AUDIOTAGS: ${md5}:${subsong}:${normalizedSubsong} ${audioTags}")
      audioTags.map(audioTag => AudioFingerprint(
        md5,
        player,
        subsong,
        normalizedSubsong,
        audioBytes,
        audioMd5,
        audioChromaprint,
        audioHash,
        audioTag,
      ))
    } else None
  }).distinct.toBuffer).get
}

/*
lazy val audioFingerprints =
  Paths.get("sources/audio").toFile.listFiles.filter(_.getName.endsWith(".tsv")).par.flatMap(tsv =>
    parseAudioTsv(tsv.getAbsolutePath, withSimHash = true)
  ).seq.distinct.toBuffer
*/

lazy val (
  audioHashesByMd5,
  components,
  duplicatesForTag,
  duplicateSubsongsByPlayerAndMd5
) = {
  val filteredAudioFingerprints =
    Paths.get("sources/audio").toFile.listFiles.filter(_.getName.endsWith(".tsv")).par.flatMap(tsv =>
      parseAudioTsv(tsv.getAbsolutePath, withSimHash = true)
    )
    .groupBy(_.audioTag).filter(_._1.nonEmpty).par.mapValues { fps =>
      if (fps.size == 1 && songlengths.songlengthsByMd5(fps.head.md5).forall(_.subsongs.size == 1)) {
        //println(s"Only one entry for audioTag ${fps.head.audioTag} md5: ${fps.head.md5}, dropping chromaprint")
        fps.map(fp => fp.copy(audioChromaprint = ""))
      } else fps
    }.values.flatten.toBuffer.seq

  val _audioHashesByMd5 = filteredAudioFingerprints.groupBy(_.md5).par.mapValues(_.sortBy(_.normalizedSubsong).map(_.audioHash).distinct).seq.toMap

  def audioFingerPrintComponents(): Iterable[Seq[(String, Buffer[AudioFingerprint], collection.immutable.Map[String, Buffer[AudioFingerprint]], List[String])]] = {
    // precompute per-audioTag data that doesn't change across passes
    val audioByAudioTags = filteredAudioFingerprints.map(e =>
      (e.audioTag, e)).groupMap(_._1)(_._2).par.mapValues(_.distinct).seq
    var audioTagData = audioByAudioTags.par.map { case (audioTag, entries) =>
      val entriesByHash = entries.groupBy(_.md5).view.mapValues(_.distinctBy(_.normalizedSubsong)).toMap
      val hashes = entries.map(_.md5).distinct.sorted.toList
      (audioTag, entries, entriesByHash, hashes)
    }.seq
    // group audioTags into connected components by shared md5s
    // audioTags in different components touch disjoint md5 sets and can run in parallel
    val audioTagKeys = audioTagData.map(_._1).toArray
    val parent = scala.collection.mutable.Map[String, String]()
    def find(x: String): String = {
      var r = x
      while (parent.getOrElse(r, r) != r) r = parent.getOrElse(r, r)
      var c = x
      while (c != r) { val n = parent.getOrElse(c, c); parent(c) = r; c = n }
      r
    }
    def union(a: String, b: String): Unit = { parent(find(a)) = find(b) }
    // build mapping: md5 -> first audioTag that uses it, then union subsequent audioTags
    val md5FirstTag = scala.collection.mutable.Map[String, String]()
    for ((audioTag, _, _, hashes) <- audioTagData; h <- hashes) {
      md5FirstTag.get(h) match {
        case Some(first) => union(audioTag, first)
        case None => md5FirstTag(h) = audioTag
      }
    }
    val audioTagDataMap = audioTagData.map(t => (t._1, t)).toMap
    audioTagKeys.groupBy(find).values.par.map(_.map(audioTagDataMap).toSeq).seq
  }

  val rawComponents = audioFingerPrintComponents()

  // precompute duplicate relationships per audioTag: for each hash, all hashes that are audio-duplicates of it
  // this is constant across all passes since it depends only on audio data
  val _duplicatesForTag = rawComponents.flatMap { component =>
    component.par.map { case (audioTag, _, entriesByHash, hashes) =>
      val hashesArr = hashes.toArray
      val entriesArr = hashesArr.map(h => entriesByHash(h).toArray)
      val numHashes = hashesArr.length

      val dupMap = hashesArr.indices.map { cmpIdx =>
        val cmpHash = hashesArr(cmpIdx)
        val cmpAudioEntries = entriesArr(cmpIdx)
        val dups = Buffer.empty[String]

        var j = 0
        while (j < numHashes && dups.size < numHashes) {
          val audioEntries = entriesArr(j)
          var duplicate = true
          var i = 0
          while (i < audioEntries.length && duplicate) {
            if (cmpAudioEntries(i).audioMd5 == audioEntries(i).audioMd5) {
              // duplicate = true
            } else if (cmpAudioEntries(i).audioChromaprint.nonEmpty && audioEntries(i).audioChromaprint.nonEmpty && cmpAudioEntries(i).audioChromaprint != audioEntries(i).audioChromaprint) {
              val threshold = if (audioEntries(i).audioBytes > 2 * 11025 * 12) 0.9 else 0.99
              val similarity = chromaSimilarity(cmpAudioEntries(i).audioChromaprint, audioEntries(i).audioChromaprint)
              if (similarity < threshold) duplicate = false
            } else if (cmpAudioEntries(i).audioHash != audioEntries(i).audioHash) {
              duplicate = false
            }
            i += 1
          }
          if (duplicate) {
            dups += hashesArr(j)
          }
          j += 1
        }
        (cmpHash, dups.toSet)
      }.toMap
      (audioTag, dupMap)
    }
  }.seq.toMap

  val _components = rawComponents.par.map { _.map { case (audioTag, _, _, hashes) =>
    (audioTag, Buffer.empty[AudioFingerprint], collection.immutable.Map.empty[String, Buffer[AudioFingerprint]], hashes)
  }}.seq

  val audioByPlayerAndMd5 = filteredAudioFingerprints.groupBy(e => (e.player, e.md5))
    .par.mapValues(_.sortBy(_.normalizedSubsong).distinct).seq
  
  val _duplicateSubsongsByPlayerAndMd5 = songlengths.db.sortBy(_.md5).par.flatMap(e => {
    val md5 = e.md5.take(12)
    val duplicates = scala.collection.mutable.SortedSet[Int]()
    val fingerprints = audioByPlayerAndMd5.get((e.player, md5)).getOrElse(Buffer.empty)
    if (fingerprints.nonEmpty) {
      val filtered = fingerprints.filter(_.audioBytes > 0).map(f => f.copy(audioTag = f.audioTag.replaceFirst(s"^[0-9]+-", ""))).distinctBy(f => (f.subsong, f.audioTag))
      val grouped = (
        if (filtered.forall(e => e.audioBytes > 2 * 11025 * 12 && e.audioBytes == filtered.head.audioBytes)) filtered.groupBy(_.audioBytes)
        else filtered.groupBy(_.audioTag)
      ).mapValues(_.distinct)
      val audioTags = filtered.map(f => (f.subsong, f.audioTag)).distinct.groupBy(_._1).mapValues(_.map(_._2).sorted.distinct).toMap
      if (!audioTags.values.forall(_.size <= 2)) {
        System.err.println(s"WARN: inconsistent audio tags for md5: $md5 player: ${e.player} format: ${e.format} audioTags: ${audioTags}")
      }
      val audioTagsIdentical = filtered.forall(_.audioBytes > 2 * 11025 * 12) && (
        (audioTags.values.forall(_ == audioTags.head._2) && e.subsongs.size > 2) ||
        audioTags.values.forall(_ == audioTags.head._2) && filtered.forall(_.audioBytes == filtered.head.audioBytes)
      )
      val baseThreshold = if (audioTagsIdentical) 0.9 else 0.99
      assert(grouped.values.forall(group => group.map(_.subsong).sorted == group.map(_.subsong)))
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
            } else if (se.audioChromaprint.nonEmpty && cmp.audioChromaprint.nonEmpty && se.audioChromaprint != cmp.audioChromaprint) {
              val threshold = (if (audioTags(se.subsong) != audioTags(cmp.subsong)) 0.995 else baseThreshold)
              val similarity = chromaSimilarity(cmp.audioChromaprint, se.audioChromaprint)
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

  chromaprint.clearCaches()

  (_audioHashesByMd5, _components, _duplicatesForTag, _duplicateSubsongsByPlayerAndMd5)
}
