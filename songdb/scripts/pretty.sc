// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._

import convert._

def songlengthLine(s: SongInfo): String = {
  def shorten(songend: String) : String = {
    if (songend == "player+silence") return "p+s"
    if (songend == "player+volume") return "p+v"
    if (songend == "loop+silence") return "l+s"
    if (songend == "loop+volume") return "l+v"
    return songend.take(1)
  }
  Buffer(
    s.hash.take(12),
    s.minsubsong.toString,
    s.subsongs.map(ss =>
      Buffer(
        ss.songlength.toString,
        shorten(ss.songend)
      ).mkString(",")
    ).mkString(" ")
  ).mkString("\t")
}

def createPrettySonglengthsTsv(songlengths: Buffer[SongInfo]) = {
  assert(songlengths.map(_.hash).distinct.size == songlengths.size)
  songlengths.sortBy(_.hash).par.map(songlengthLine(_)).mkString("\n").concat("\n")
}

def parsePrettySonglengthsTsv(tsv: String) = {
  def decodeSongend(songend: String) : String = songend match {
    case "e" => "error"
    case "p" => "player"
    case "t" => "timeout"
    case "s" => "silence"
    case "l" => "loop"
    case "v" => "volume"
    case "r" => "repeat"
    case "p+s" => "player+silence"
    case "p+v" => "player+volume"
    case "l+s" => "loop+silence"
    case "l+v" => "loop+volume"
    case "n" => "nosound"
    case _ => assert(false)
  }
  val songlengths = tsv.split('\n').par.map { s =>
    val cols = s.split("\t", -1)
    val hash = cols(0)
    val minsubsong = cols(1).toInt
    val subsongs = cols(2).split(' ').map { ss =>
      val e = ss.split(',')
      SubsongInfo(
        songlength = e(0).toInt,
        songend = decodeSongend(e(1))
      )
    }
    SongInfo(hash, minsubsong, subsongs.toBuffer)
  }
  assert(songlengths.map(_.hash).distinct.size == songlengths.size)
  songlengths.toBuffer.sortBy(_.hash)
}

def modinfoLine(m: ModInfo): String = {
  Buffer(
    m.hash.take(12),
    m.format,
    if (m.channels > 0) m.channels.toString else ""
  ).mkString("\t")
}

def createPrettyModInfosTsv(modinfos: Buffer[ModInfo]) = {
  assert(modinfos.map(_.hash).distinct.size == modinfos.size)
  modinfos.sortBy(_.hash).par.map(modinfoLine(_)).mkString("\n").concat("\n")
}

def parsePrettyModInfosTsv(tsv: String) = {
  val modinfos = tsv.split('\n').par.map { m =>
    val cols = m.split("\t", -1)
    ModInfo(
      hash = cols(0),
      format = cols(1),
      channels = if (!cols(2).isEmpty) cols(2).toInt else 0
    )
  }
  assert(modinfos.map(_.hash).distinct.size == modinfos.size)
  modinfos.toBuffer.sortBy(_.hash)
}

def metaLine(m: MetaData): String = {
  Buffer(
    m.hash.take(12),
    m.authors.mkString(SEPARATOR),
    m.publishers.mkString(SEPARATOR),
    m.album,
    if (m.year > 0) m.year.toString else ""
  ).mkString("\t")
}

def createPrettyMetaTsv(meta: Buffer[MetaData]) = {
  assert(meta.map(_.hash).distinct.size == meta.size)
  meta.sortBy(_.hash).par.map(metaLine(_)).mkString("\n").concat("\n")
}

def parsePrettyMetaTsv(tsv: String) = {
  val infos = tsv.split('\n').par.map { i =>
    val cols = i.split("\t", -1)
    MetaData(
      hash = cols(0),
      authors = if (!cols(1).isEmpty) cols(1).split(SEPARATOR).toBuffer else Buffer.empty,
      publishers = if (!cols(2).isEmpty) cols(2).split(SEPARATOR).toBuffer else Buffer.empty,
      album = cols(3),
      year = if (!cols(4).isEmpty) cols(4).toInt else 0
    )
  }
  assert(infos.map(_.hash).distinct.size == infos.size)
  infos.toBuffer.sortBy(_.hash)
}
