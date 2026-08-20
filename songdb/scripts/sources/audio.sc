// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025-2026 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import java.nio.file.Paths
import scala.util.Using
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._

import chromaprint._
import songlengths._

val persecondbytes = 2 * 11025

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
) {
  lazy val effectiveAudioBytes: Int = {
    if (audioChromaprint.isEmpty) audioBytes
    else {
      // only available when the fingerprint was cached (withSimHash=true, songdb.sc path)
      val fp = fpByHash(audioChromaprint)
      if (fp == null) throw new IllegalStateException("effectiveAudioBytes requires a cached fingerprint (withSimHash=true)")
      if (fp.length == 0) audioBytes
      else {
        val (s, e) = fp.contentBounds
        (audioBytes.toLong * (e - s) / fp.length).toInt
      }
    }
  }
}

val audioTsvSizes = Map(
  "sources/audio/audio_0.tsv" -> 162244219L,
  "sources/audio/audio_1.tsv" -> 164567239L,
  "sources/audio/audio_2.tsv" -> 163394653L,
  "sources/audio/audio_3.tsv" -> 163523252L,
  "sources/audio/audio_4.tsv" -> 163229718L,
  "sources/audio/audio_5.tsv" -> 162149062L,
  "sources/audio/audio_6.tsv" -> 162768888L,
  "sources/audio/audio_7.tsv" -> 164877542L,
  "sources/audio/audio_8.tsv" -> 162623874L,
  "sources/audio/audio_9.tsv" -> 163775618L,
  "sources/audio/audio_a.tsv" -> 160023725L,
  "sources/audio/audio_b.tsv" -> 164023188L,
  "sources/audio/audio_c.tsv" -> 164255398L,
  "sources/audio/audio_d.tsv" -> 162465611L,
  "sources/audio/audio_e.tsv" -> 162917749L,
  "sources/audio/audio_f.tsv" -> 164257309L
)

def parseAudioTsv(tsv: String, withSimHash: Boolean, md5s: Set[String] = Set.empty, lengths: Set[Int] = Set.empty) = {
  var prevMd5 = ""
  var prevPlayer = ""
  var fixsubsong = false
  
  val f = Paths.get(tsv).toFile
  val key = tsv.split("/").takeRight(3).mkString("/")
  if (f.length() != audioTsvSizes(key)) {
    System.err.println()
    System.err.println()
    System.err.println(s"ERROR: audio TSV file ${tsv} has unexpected size ${f.length()} (expected ${audioTsvSizes(key)})")
    System.err.println()
    System.err.println(s"Make sure the audio TSV files are decompressed correctly from the zstd archives in 'sources/audio' (e.g. zstd -d sources/audio/audio_*.zst)")
    System.err.println(s"And that you are using the latest version of the files. The source code and the audio TSV files must be in sync.")
    System.err.println(s"See README.md for instructions.")
    System.exit(1)
  }
  Using(scala.io.Source.fromFile(f))( _.getLines().toBuffer.flatMap(line => {
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
    if (audioBytes > 0 && (md5s.isEmpty || md5s.contains(md5)) && (lengths.isEmpty || lengths.exists(len => Math.abs(audioBytes.toDouble / persecondbytes - len.toDouble / persecondbytes) <= 3.0))) {
      val audioMd5 = if (l.length >= 5) l(4).take(12) else ""
      val audioChromaprint = if (l.length >= 6) l(5) else ""
      // withSimHash (songdb.sc): decode + cache the fingerprint under its xxhash64 key, keeping only the compact key
      // (the full base64 chromaprint string is not retained in memory after this point)
      // without withSimHash (find_dupes.sc/audio_match.sc): keep the base64 string, no caching needed
      val audioChromaprintHash = if (withSimHash && audioChromaprint.nonEmpty) cacheChromaprint(audioChromaprint) else audioChromaprint
      // require at least 9s of audio for simhash comparison to minimize false positives
      val (audioSimHash, simTags) = if (withSimHash && audioChromaprint.nonEmpty && audioBytes > persecondbytes * 9) {
        val fp = fpByHash(audioChromaprintHash) : @unchecked
        val numHashes = Math.max(1, audioBytes / (persecondbytes * 3)) // one hash per 3s of audio
        val h = SimHash(fp.data, numHashes)
        val hex = h.toString(16)
        (hex, Seq((h.bitLength+1)/4, (h.bitLength-1)/4).distinct)
      } else ("",Seq.empty[Int])
      val audioHash = Seq(audioSimHash, audioChromaprintHash, audioMd5).filter(_.nonEmpty).head
      val audioTags =
        if (withSimHash) {
          // add tags based on simhash (if available) or audio hash
          (if (audioHash == audioSimHash) simTags.map(t => player + "-h-" + t) else Seq(player + "-h-" + audioHash)) ++
          // add tags based on songlength (1s precision)
          (if (audioBytes > persecondbytes * 9) Seq(player + "-l-" + ((audioBytes + persecondbytes/4) / (persecondbytes)), player + "-l-" + ((audioBytes - persecondbytes/4) / (persecondbytes))).distinct else Seq.empty)
        } else Seq("")
      audioTags.map(audioTag => AudioFingerprint(
        md5,
        player,
        subsong,
        normalizedSubsong,
        audioBytes,
        audioMd5,
        audioChromaprintHash,
        audioHash,
        audioTag,
      ))
    } else None
  }).distinct.toBuffer).get.seq
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

  def audioFingerPrintComponents(): Iterable[Seq[(String, List[String])]] = {
    // precompute per-audioTag data that doesn't change across passes
    val audioByAudioTags = filteredAudioFingerprints.map(e =>
      (e.audioTag, e)).groupMap(_._1)(_._2).par.mapValues(_.distinct).seq
    var audioTagData = audioByAudioTags.par.map { case (audioTag, entries) =>
      val hashes = entries.map(_.md5).distinct.sorted.toList
      (audioTag, hashes)
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
    for ((audioTag, hashes) <- audioTagData; h <- hashes) {
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
  val allSubsongDataByMd5 = filteredAudioFingerprints.groupBy(_.md5).par.mapValues(fps => {
    fps.groupBy(_.normalizedSubsong).toSeq.sortBy(_._1).map { case (_, subsongFps) =>
      val tags = subsongFps.map(_.audioTag).distinct
      (tags, subsongFps)
    }.toArray
  }).seq.toMap

  val knownMedleyMd5s = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
  val fullMatchPairs = java.util.concurrent.ConcurrentHashMap.newKeySet[(String, String)]()

  val rawDuplicatesForTag = rawComponents.flatMap { component =>
    component.groupBy { case (_, hashes) => hashes.toSet }.values.toList.par.flatMap { group =>
      val representative = group.head
      val hashes = representative._2
      val hashesArr = hashes.toArray
      val entriesArr = hashesArr.map(h => allSubsongDataByMd5(h))
      val numHashes = hashesArr.length
      val parent = Array.tabulate(numHashes)(identity)
      def find(x: Int): Int = {
        var r = x
        while (parent(r) != r) r = parent(r)
        var c = x
        while (c != r) { val n = parent(c); parent(c) = r; c = n }
        r
      }
      def union(a: Int, b: Int): Unit = { parent(find(a)) = find(b) }
      val subsetsOf = scala.collection.mutable.Map[Int, scala.collection.mutable.Buffer[Int]]()
      val duplicatePairs = scala.collection.mutable.HashSet[(Int, Int)]()

      var cmpIdx = 0
      while (cmpIdx < numHashes) {
        val cmpHash = hashesArr(cmpIdx)
        val cmpSubsongs = entriesArr(cmpIdx)
        val validCmp = cmpSubsongs.filter(_._2.exists(_.effectiveAudioBytes > 0))
        val cmpLen = validCmp.length
        
        var j = cmpIdx + 1
        while (j < numHashes) {
          if (find(cmpIdx) != find(j)) {
            val subHash = hashesArr(j)
            val subsongs = entriesArr(j)
            val validSub = subsongs.filter(_._2.exists(_.effectiveAudioBytes > 0))
            val subLen = validSub.length
            
            var duplicate = true
          
            val smaller = if (cmpLen <= subLen) validCmp else validSub
            val larger = if (cmpLen <= subLen) validSub else validCmp
            
            val smallerAudioBytes = smaller.map(_._2.head.effectiveAudioBytes).toList
            var largerAudioBytes = larger.map(_._2.head.effectiveAudioBytes).toList
            val isSubset = smallerAudioBytes.forall(b => {
              val idx = largerAudioBytes.indexOf(b)
              if (idx >= 0) { largerAudioBytes = largerAudioBytes.patch(idx, Nil, 1); true } else false
            })
            
            val requiredStrictMatches = if (isSubset || smaller.length == 1) 1 else 2

            var i = 0
            var strictMatchCount = 0
            val matchedLarger = scala.collection.mutable.Set[Int]()
            val matchedSmaller = scala.collection.mutable.Set[Int]()
            while (i < smaller.length && strictMatchCount < requiredStrictMatches && (smaller.length - i) + strictMatchCount >= requiredStrictMatches) {
              val (cmpTags, cmpFps) = smaller(i)
              var k = 0
              val initialMatchCount = strictMatchCount
              
              while (k < larger.length && strictMatchCount == initialMatchCount) {
                val (tags, fps) = larger(k)
                val commonTags = cmpTags.intersect(tags)
                  
                if (commonTags.nonEmpty) {
                  val cmpFp = cmpFps.find(f => f.audioTag == commonTags.head).get
                  val fp = fps.find(f => f.audioTag == commonTags.head).get
                  val (realCmpFp, realSubFp) = if (cmpLen <= subLen) (cmpFp, fp) else (fp, cmpFp)
                    
                  var isStrictMatch = true
                  if (cmpFp.audioMd5 == fp.audioMd5 && cmpFp.audioMd5.nonEmpty) {
                    // duplicate = true
                  } else if (cmpFp.audioChromaprint.nonEmpty && fp.audioChromaprint.nonEmpty && cmpFp.audioChromaprint != fp.audioChromaprint) {
                    val cmpseconds = cmpFp.effectiveAudioBytes.toDouble / persecondbytes
                    val fpseconds = fp.effectiveAudioBytes.toDouble / persecondbytes
                    val threshold = if (cmpseconds >= 10 && fpseconds >= 10) Math.max(0.999 - 0.01 * Math.min(cmpseconds - 10, fpseconds - 10), 0.82) else 0.999
                    val similarity = chromaSimilarity(cmpFp.audioChromaprint, fp.audioChromaprint)
                    if (similarity < threshold) {
                      isStrictMatch = false
                    }
                  } else if (cmpFp.audioHash != fp.audioHash) {
                    isStrictMatch = false
                  } 
                  if (isStrictMatch && !matchedSmaller.contains(i) && !matchedLarger.contains(k)) {
                    strictMatchCount += 1
                    matchedLarger += k
                    matchedSmaller += i
                  }
                }
                k += 1
                while (matchedLarger.contains(k)) k += 1
              }
              i += 1
            }
            if (duplicate && strictMatchCount < requiredStrictMatches) {
              duplicate = false
            }
            if (duplicate) {
              duplicatePairs += ((cmpIdx, j))
              if (cmpLen == subLen) {
                union(cmpIdx, j)
                if (strictMatchCount == cmpLen) {
                  fullMatchPairs.add((cmpHash, subHash))
                  fullMatchPairs.add((subHash, cmpHash))
                }
              } else {
                val largerIdx = if (cmpLen > subLen) cmpIdx else j
                val smallerIdx = if (cmpLen > subLen) j else cmpIdx
                subsetsOf.getOrElseUpdate(largerIdx, Buffer.empty) += smallerIdx
              }
            }
          }
          j += 1
        }
        cmpIdx += 1
      }

      for ((largerIdx, subsets) <- subsetsOf) {
        var isMedley = false
        var aIdx = 0
        while (aIdx < subsets.length && !isMedley) {
          var bIdx = aIdx + 1
          while (bIdx < subsets.length && !isMedley) {
            val a = subsets(aIdx)
            val b = subsets(bIdx)
            if (find(a) != find(b)) {
              val minIdx = if (a < b) a else b
              val maxIdx = if (a < b) b else a
              if (!duplicatePairs.contains((minIdx, maxIdx))) {
                isMedley = true
              }
            }
            bIdx += 1
          }
          aIdx += 1
        }
        if (!isMedley) {
          for (sub <- subsets) union(largerIdx, sub)
        } else {
          System.err.println(s"INFO: Detected medley for ${hashesArr(largerIdx)} with subsets: ${subsets.map(hashesArr).mkString(", ")}")
          knownMedleyMd5s.add(hashesArr(largerIdx))
        }
      }

      val lenMap = hashesArr.indices.map(i => hashesArr(i) -> entriesArr(i).count(_._2.exists(_.effectiveAudioBytes > 0))).toMap
      val dupSets = hashesArr.indices.groupBy(find).values.map(_.map(hashesArr).toSet).toSeq
      val dupMap = dupSets.flatMap(set => set.map(h => h -> set.filter(x => lenMap(h) >= lenMap(x)))).toMap
      group.map { case (audioTag, _) =>
        (audioTag, dupMap)
      }
    }
  }.seq.toMap

  val _duplicatesForTag: Map[String, Map[(String, Boolean), Set[String]]] = rawDuplicatesForTag.map { case (tag, dupMap) =>
    tag -> dupMap.flatMap { case (k, v) =>
      Seq(
        (k, true) -> {
          if (knownMedleyMd5s.contains(k))
            v.filter(m => m == k || (knownMedleyMd5s.contains(m) && fullMatchPairs.contains((k, m))))
          else
            v.filterNot(knownMedleyMd5s.contains)
        },
        (k, false) -> v
      )
    }
  }

  val audioByPlayerAndMd5 = filteredAudioFingerprints.groupBy(e => (e.player, e.md5))
    .par.mapValues(_.sortBy(_.normalizedSubsong).distinct).seq
  
  val _duplicateSubsongsByPlayerAndMd5 = songlengths.db.sortBy(_.md5).par.flatMap(e => {
    val md5 = e.md5.take(12)
    val duplicates = scala.collection.mutable.SortedSet[Int]()
    val fingerprints = audioByPlayerAndMd5.get((e.player, md5)).getOrElse(Buffer.empty)
    if (fingerprints.nonEmpty) {
      val filtered = fingerprints.filter(f => f.effectiveAudioBytes > 0 && f.audioTag.contains("-h-")).distinctBy(f => (f.subsong, f.audioTag))
      val grouped = (
        if (filtered.forall(e => filtered.head.effectiveAudioBytes > persecondbytes * 12 && e.effectiveAudioBytes > persecondbytes * 12 && e.audioBytes == filtered.head.audioBytes)) filtered.groupBy(_.audioBytes)
        else filtered.groupBy(_.audioTag)
      ).mapValues(_.distinct)
      val audioTags = filtered.map(f => (f.subsong, f.audioTag)).distinct.groupBy(_._1).mapValues(_.map(_._2).sorted.distinct).toMap
      if (!audioTags.values.forall(_.size <= 2)) {
        System.err.println(s"WARN: inconsistent audio tags for md5: $md5 player: ${e.player} format: ${e.format} audioTags: ${audioTags}")
      }
      val audioTagsIdentical = filtered.forall(_.effectiveAudioBytes > persecondbytes * 12) && (
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
              val similarity = chromaSimilarity(cmp.audioChromaprint, se.audioChromaprint, matchSilence = true)
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

  val _audioHashesByMd5 = filteredAudioFingerprints.groupBy(_.md5).par.mapValues(_.sortBy(_.normalizedSubsong).map(_.audioHash).distinct).seq.toMap

  val _subsongCountsByMd5 = allSubsongDataByMd5.map { case (md5, subsongs) =>
    md5 -> subsongs.count(_._2.exists(_.effectiveAudioBytes > 0))
  }.toMap

  val _components = rawComponents.par.map { _.map { case (audioTag, hashes) =>
    val sortedHashes = hashes.map(h => (h, _subsongCountsByMd5(h))).sortBy(h => (-h._2, h._1))
    (audioTag, sortedHashes)
  }}.seq

  chromaprint.clearCaches()

  (_audioHashesByMd5, _components, _duplicatesForTag, _duplicateSubsongsByPlayerAndMd5)
}
