// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>

import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._

import convert._
import pretty._
import md5._
import sources._

lazy val md5ToXxh32 =
  sources.tsvs.par.flatMap(_._2).map { case (md5, subsongs) =>
    (md5.take(12),
    subsongs.head.xxh32 + f"${subsongs.head.filesize & 0xFFFF}%04x")
  }.seq.toMap

lazy val xxh32ToMd5 = md5ToXxh32.groupBy(_._2).map { case (xxh32, md5Pairs) =>
  (xxh32, md5Pairs.map(_._1).toSet)
}

val xxh32DupWhiteList = Set(
  "658fccfa111a", // 0e56f0f934a32dded93bac4760b50729 vs 185eefd02c5915e69013f657e0cae0af
  "799845b9d94e", // 25f8380fbda64f0a1a6c60f86c2380f6 vs fdfc919a3f4cd0751cba55ae80883b62
  "326bf0e1bcc3", // 1076e7fabdef36590be65aab24feec34 vs f8802da4ead54a13e7dbf747501b4fec
  "52c3a0b7c6a6", // 9e321ee91ef7d292a9080c4e185f0556 vs c3e2cc9338972366c319c304b46923c4
  "94ba90070307", // 109cd82cef79dfd10113aabfb715c1e4 vs d2a2479178142e3f73357144aed233f7
  "00a65024e38c", // 0bc4469929ab20585382d2d60c24e23c vs c8a0f91fdc060b993599b31afa715d6d
  "a52b32727158", // bc98ee68393b4816c5f1341d4a851cf6 vs d0028ee626e81b7074a6c4c80a0d1e87
  "9b5752980f4e", // 674b25511b3bbafdad37030a8630fdfd vs 3aa9ff3d1bd5d6fe5687004ffef4fdc3
  "330b37fdbc1d", // 91d2efefd99cbb9e7c53aef09903d25d vs f43a6475e9700db56786479d792675ea
  "f043f90d4bc0", // f98d02e1a580828bb8923c4e16d93149 vs 84054586870e79817aa597123b7aeb43
)

def songlengthsToXxh32(songlengths: Buffer[SongInfo]) = {
  assert(songlengths.map(_.hash).distinct.size == songlengths.size)
  val xxh = songlengths.map(e => e.copy(hash = md5ToXxh32(e.hash)))
  xxh.groupBy(_.hash).map { case (_, entries) =>
    try {
      assert(entries.forall(e => e == entries.head || xxh32DupWhiteList.contains(e.hash)))
    } catch {
      case e: AssertionError =>
        System.err.println(s"ERROR: Inconsistent songlength entries for xxh32 ${entries.head.hash} md5s ${xxh32ToMd5(entries.head.hash)}: ${entries}")
        throw e
    }
    entries.head
  }.toBuffer
}

def modinfosToXxh32(modinfos: Buffer[ModInfo]) = {
  assert(modinfos.map(_.hash).distinct.size == modinfos.size)
  val xxh = modinfos.map(e => e.copy(hash = md5ToXxh32(e.hash)))
  xxh.groupBy(_.hash).map { case (_, entries) =>
    try {
      assert(entries.forall(e => e == entries.head || xxh32DupWhiteList.contains(e.hash)))
    } catch {
      case e: AssertionError =>
        System.err.println(s"ERROR: Inconsistent modinfo entries for xxh32 ${entries.head.hash} md5s ${xxh32ToMd5(entries.head.hash)}: ${entries}")
        throw e
    }
    entries.head
  }.toBuffer
}

def metasToXxh32(meta: Buffer[MetaData]) = {
  assert(meta.map(_.hash).distinct.size == meta.size)
  val xxh = meta.map(e => e.copy(hash = md5ToXxh32(e.hash)))
  xxh.groupBy(_.hash).par.map { case (_, entries) =>
    if (!entries.forall(_ == entries.head)) {
      // select best based on some ad hoc metadata heuristics
      val scores = entries.map(e => (e, e.publishers.size + (if (e.album.nonEmpty) 1 else 0) + (if (e.year > 0) 1 else 0))).toMap
      val bestscore = scores.maxBy(_._2)._2
      val bestentries = entries.filter(e => (e.publishers.size + (if (e.album.nonEmpty) 1 else 0) + (if (e.year > 0) 1 else 0)) == bestscore)
      val minyear = bestentries.map(e => if (e.year > 0) e.year else 9999).min
      val byyear = bestentries.filter(_.year == minyear)
      var best = if (byyear.nonEmpty) byyear.maxBy(_.authors.size) else bestentries.maxBy(_.authors.size)
      if (best.authors.isEmpty) {
        best = best.copy(authors = bestentries.maxBy(_.authors.size).authors)
      }
      System.err.println(s"WARN: Conflicting meta data entries for xxh32 ${entries.head.hash} md5s ${xxh32ToMd5(entries.head.hash)}: ${entries}, best: ${best}")
      best.copy(hash = entries.head.hash)
    } else entries.head
  }.seq.toBuffer
}

val _xxh32check = scala.collection.mutable.Map[String,String]()
val _xxh32idx = scala.collection.mutable.Map[String,String]()
val _idxxxh32 = scala.collection.mutable.Map[String,String]()

def xxh32(xxh32: String) = synchronized {
  val base64 = base64e(xxh32)
  if (_xxh32check.contains(base64) && _xxh32check(base64) != xxh32) {
    System.err.println(s"ERROR: XXH32 check failed, short ${base64} existing ${_xxh32check(base64)} new ${xxh32}")
    throw new IllegalStateException
  }
  _xxh32check(base64) = xxh32
  val xxh32v = java.lang.Long.parseLong(xxh32.take(12), 16)
  if (base64d(base64) != xxh32v) {
    System.err.println(s"ERROR: XXH32 vs base64 check failed for XXH32:${xxh32.take(12)}/${xxh32v} base64: ${base64}/${base64d(base64)}")
    throw new IllegalStateException
  }
  base64
}
