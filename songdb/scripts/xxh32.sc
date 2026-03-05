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
  "728d535a6168", // 09e58b5ced527047bb209ca9cfbbeced vs 72c2d9b7817e0642f02372dd24a7b0ec
  "5e151636c8ae", // 02e6ac2209a476bf3d1abcb587bdd1fe vs 7370e7cc32e1d359fca348726c4eb81a
  "1b0bf529bc3c", // c9a4f5d4d6c645aefdabd74d557729be vs 924c9d32b91d45bcd6e64a35faad6bf2
  "7f45b20c2063", // b0dd3049a926414091114cc103820b87 vs 1e726736460d94a1c2fe9579c143fd58
  "3a44d245a550", // 2fabf5e8f39df6ae616107eca04d995c vs d3c69e0f5c42e97b002b02a01ef8fb86
  "4037ef7b0123", // c75ce98c4a26ef515b83bc2ec3489536 vs 637cb45758c1ec4e453254768b4c94e2
  "d6b5a65088fd", // 5d2e6751231dfc7bfb762a81369f984b vs 3881baeece91b3a3ac680965bfe2d52b
  "efd081d569de", // 58e55167b0dbb3b58e6d4afa525c63b4 vs 727288478afeefa06c5014cfb7ebb766
  "f7e3ca7950b1", // 5d962e010a3d159a94b523c4440b20bd vs b67b7fe9f240d746b6fed59f2d26aece
  "1b0bf529bc45", // c7c9f03e9bff4e228da20a48f0b12c28 vs 4be9fd4da439deea320acdfb18f759e7
  "95d9578394b5", // 49d5f2b6bf2615ccf94ece5f8ea3a00e vs e4578a273eb0f054c87bdd38dd5818b9
  "3de0b3f43c57", // 303ccf582938c40a9f9152a947963178 vs 73fa94ff120591858533f8989d0ec551
  "3c9060b7949f", // 7b5c2f962a8bb688320ab5fdd65f70db vs 29f78851de9fb4c5829930683370db4d
  "0ad487eec7af", // bbd5e4cedd882b00fd55efbaaf5ba15e vs a2424f3a8b522e918be0193d1f35e768
  "cf7ea0252332", // 880139cce6a913366e8d2bb6a215e31a vs b525b34f85ce66233ee8318a3f763e2b
  "06d624e80137", // ec7a94d4a168829342a08afc21d775b4 vs f7c5b7d021cccf757767f8099fc5dc02
  "dc221903fae0", // d7b175adb59ad149712b7c6b2e5498f7 vs afca85f41859273f184418694a6c6ad2
  "0ea3a79c9cc2", // 71b989b4c26f97ece653b1fbce8cbe67 vs f16cebbf7b212a4f6626bc06e930800c vs 0cfe699f2da92e729ef984ed3c1d206c
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
