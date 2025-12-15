// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._

case class Subsong (
  subsong: Int,
  songlength: Int,
  songend: String,
)

case class SonglengthEntry (
  md5: String,
  minsubsong: Int,
  maxsubsong: Int,
  subsongs: Seq[Subsong],
  player: String,
  format: String,
  channels: Int,
)

lazy val db = sources.tsvs.par.flatMap(_._2).map({case (md5,_subsongs) => {
  if (!_subsongs.forall(_.player == _subsongs.head.player)) {
    System.err.println("WARN: inconsistent players for " + md5 + ": " + _subsongs)
  }
  val subsongs = if (_subsongs.exists(_.player == "uade")) _subsongs.filter(_.player == "uade") else _subsongs.filter(_.player == _subsongs.head.player)
  val minsubsong = subsongs.minBy(_.subsong).subsong
  val maxsubsong = subsongs.maxBy(_.subsong).subsong
  val songs = subsongs.map(s => Subsong(s.subsong, s.songlength, s.songend)).distinct.toSeq
  val e = subsongs.filterNot(_.format.isEmpty)
  val player = e.headOption.map(_.player).getOrElse("")
  val format = e.headOption.map(e => if (e.format != "???") e.format else "").getOrElse("")
  val channels = e.headOption.map(_.channels).getOrElse(0)
  if (songs.length > maxsubsong - minsubsong + 1) {
      System.err.println("WARN: inconsistent songlengths for " + md5 + ": " + songs)
      val subsongs = Buffer.empty[Subsong]
      for (subsong <- minsubsong to maxsubsong) {
        subsongs.append(songs.filter(_.subsong == subsong).maxBy(_.songlength))
      }
      SonglengthEntry(md5, minsubsong, maxsubsong, subsongs.toSeq, player, format, channels)
  } else {
    SonglengthEntry(md5, minsubsong, maxsubsong, songs, player, format, channels)
  }
}}).distinct.groupBy(_.md5).map({case (md5, _entries) =>
  if (!_entries.forall(_.player == _entries.head.player)) {
    System.err.println("WARN: inconsistent players for " + md5 + ": " + _entries)
  }
  val entries = if (_entries.exists(_.player == "uade")) _entries.filter(_.player == "uade") else _entries.filter(_.player == _entries.head.player)
  var best = entries.head
  if (entries.length > 1) {
    val totallens = entries.map(_.subsongs.map(_.songlength).sum)
    val minmax = entries.groupBy(e => (e.minsubsong, e.maxsubsong))
    if (totallens.exists(_ != totallens.head) || minmax.size > 1) {
      System.err.println("WARN: inconsistent songlengths: " + entries)
    }
    val maxsubsongs = entries.maxBy(_.subsongs.size).subsongs.size
    best = entries.filter(_.subsongs.size == maxsubsongs).maxBy(e => e.subsongs.map(_.songlength).sum / e.subsongs.length)
  }
  best
}).toSeq.seq

lazy val songlengthsByMd5 = db.groupBy(_.md5.take(12)).par.mapValues(_.distinct).seq
