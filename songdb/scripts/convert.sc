// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map
import scala.collection.parallel.CollectionConverters._
import scala.reflect.ClassTag
import scala.util.Using

import md5._
import dedup._

val SEPARATOR = "\u007E" // ~
val SORT = "\u0001"

case class SubsongInfo (
  songlength: Int, // in ms
  songend: String,
  duplicate: Boolean,
)

trait BaseInfo {
  def hash: String // 48-bit hash
  def copyWithHash(newHash: String): BaseInfo
}

case class SongInfo (
  override val hash: String,
  minsubsong: Int,
  subsongs: Buffer[SubsongInfo],
) extends BaseInfo {
  override def copyWithHash(newHash: String) = copy(hash = newHash)
  override def toString = s"SongInfo(${hash},${minsubsong},${subsongs.mkString(",")})"
}

case class ModInfo (
  override val hash: String,
  format: String, // tracker or player name (freeform)
  channels: Int, // channels or 0 if not known/defined
) extends BaseInfo {
  override def copyWithHash(newHash: String) = copy(hash = newHash)
}

case class MetaData (
  override val hash: String,
  authors: Buffer[String],
  publishers: Buffer[String],
  album: String,
  year: Int,
) extends BaseInfo {
  override def copyWithHash(newHash: String) = copy(hash = newHash)
  override def toString =
    s"MetaData(${hash},${authors.mkString(",")},${publishers.mkString(",")},${album},${year})"
}

def hashidxdiff(entries: Iterable[Buffer[String]]) = {
  var prevhash = 0
  var prevtail = Buffer.empty[String]
  entries.map(e => {
    val tail = e.tail
    val hashv = base64d24(e(0))
    val res = if (tail == prevtail || tail.isEmpty) {
      assert(hashv > prevhash)
      val diff = hashv - prevhash
      Buffer(base64e24(diff)) ++ e.tail
    } else {
      e
    }
    prevhash = hashv
    prevtail = tail
    res
  })
}

def decodeHashIdxTsv(tsv: String) = {
  val idx2hash = Buffer[String]()
  val rows = tsv.split('\n')
  assert(rows(0) == base64e(0)) // 0 entry is special
  var prevhash = 0L
  rows.tail.foreach { b64 =>
    val hash = prevhash + base64d(b64)
    assert(prevhash == 0 || hash > prevhash)
    idx2hash += f"${hash}%012x"
    prevhash = hash
  }
  idx2hash
}

def encodeHashIdxTsv(idx2hash: Buffer[String]) = {
  val res = Buffer(base64e(idx2hash.head)) // 0 entry is special
  var prev = 0L
  idx2hash.tail.foreach { hash =>
    val b64 = base64e(hash)
    val hashv = base64d(b64)
    val diff = base64e(hashv - prev, true)
    assert(diff.length() <= 6)
    assert(diff.length() >= 1)
    prev = hashv
    res += diff
  }
  res.mkString("\n").concat("\n")
}

def decodeSonglengthsTsv(tsv: String, idx2hash: Buffer[String]) = {
  assert(idx2hash.nonEmpty)
  def decodeRow(cols: Array[String], hash: String) = {
    def decodeSongend(songend: String) : String = songend match {
      case "e" => "error"
      case "p" => "player"
      case "t" => "timeout"
      case "s" => "silence"
      case "l" => "loop"
      case "v" => "volume"
      case "r" => "repeat"
      case "b" => "player+silence"
      case "P" => "player+volume"
      case "i" => "loop+silence"
      case "L" => "loop+volume"
      case "n" => "nosound"
      case _ => assert(false)
    }
    val subsongs = Buffer.empty[SubsongInfo]
    val minsubsong = if (cols.length == 1 || cols(1).isEmpty) 1 else cols(1).toInt

    var prev = SubsongInfo(0, "error", false)
    cols(0).split(" ", -1).foreach(ss =>
      if (ss.isEmpty()) {
        assert(prev != null)
        prev = prev.copy(duplicate = false)
      } else if (ss == "!") {
        assert(prev != null)
        prev = prev.copy(duplicate = true)
      } else {
        val e = ss.split(',')
        val songlength =
          if (e(0).isEmpty) 0
          else base64d24(e(0)) * 20 // 20ms accuracy in encoded tsv
        val songend =
          if (e.length == 1 || e(1).isEmpty || e(1) == "!") "player"
          else decodeSongend(e(1))
        val duplicate =
          if (e.length == 1 && e(0) == "!") true
          else if (e.length == 2 && e(1) == "!") true
          else if (e.length > 2 && e(2) == "!") true
          else false
        val subsong = SubsongInfo(songlength, songend, duplicate)
        prev = subsong
      }
      subsongs += prev
    )
    SongInfo(hash, minsubsong, subsongs)
  }
  val songlengths = tsv.split('\n').zipWithIndex.par.map {
    // 0 entry is special in md5 list
    case (row, index) => decodeRow(row.split('\t'), idx2hash(index + 1))
  }
  assert(songlengths.map(_.hash).distinct.size == songlengths.size)
  songlengths.toBuffer.sortBy(_.hash)
}

def encodeSonglengthsTsv(songlengths: Buffer[SongInfo], _check: Map[String, String]) = {
  assert(songlengths.map(_.hash).distinct.size == songlengths.size)
  def encode(songend: String) : String = songend match {
      case "player+silence" => "b"
      case "player+volume" => "P"
      case "loop+silence" => "i"
      case "loop+volume" => "L"
      case _ => songend.take(1)
  }
  val entries = songlengths.sortBy(_.hash).par.map(e =>
    var prev = ""
    val subsongs = e.subsongs.map(s =>
      val _songend = if (s.songend != "player") encode(s.songend) else ""
      val sl = if (s.songlength > 0 && s.songlength < 20) 20 else s.songlength
      val _songlength = ""+base64e24((sl + 10) / 20) // 20ms accuracy in songlengths
      var next = if (_songend.isEmpty) _songlength else _songlength + "," + _songend
      if (prev == next) {
        if (s.duplicate) "!"
        else ""
      } else {
        prev = next
        if (s.duplicate) next + ",!"
        else next
      }
    )
    val minsubsong = if (e.minsubsong != 1) e.minsubsong.toString else ""
    Buffer(e.hash.take(12), subsongs.mkString(" "), minsubsong)
  ).map(_ match {
    case Buffer(hash, "", "") => Buffer(hash)
    case Buffer(hash, subsongs, "") => Buffer(hash, subsongs)
    case b => b
  }).seq
  val dedupped = dedup(entries, "songlengths.tsv", _check)
  validate(dedupped, "songlengths.tsv")
  // drop hash as line # == hashidx
  dedupped.map(b => b.tail.mkString("\t")).mkString("\n").concat("\n")
}

def decodeTsv[T <: BaseInfo : ClassTag] (
  tsv: String,
  idx2hash: Buffer[String],
  decodeRow: (Array[String], String, T) => T,
  initial: T
): Buffer[T] = {
  assert(idx2hash.nonEmpty)
  var idx = 0
  var info = initial
  var i = 0
  val split = tsv.split('\n')
  val infos = split.zipWithIndex.map {
    case (row, index) =>
      val cols = row.split('\t')
      val next_idx = base64d24(cols(0))
      if (cols.length == 1) {
        idx += next_idx
        info = info.copyWithHash(idx2hash(idx)).asInstanceOf[T]
      } else {
        idx = next_idx
        info = decodeRow(cols, idx2hash(idx), info)
      }
      i += 1
      assert(idx < idx2hash.size)
      info
  }
  assert(infos.map(_.hash).distinct.size == split.size)
  infos.toBuffer.sortBy(_.hash)
}

def decodeModInfosTsv(tsv: String, idx2hash: Buffer[String]) = {
  def decodeRow(cols: Array[String], hash: String, prev: ModInfo) = {
    val format = if (cols.length > 1) {
      if (cols(1) == REPEAT) prev.format
      else cols(1)
    } else ""

    val channels = if (cols.length > 2) {
      if (cols(2) == REPEAT) prev.channels
      else cols(2).toInt
    } else 0

    ModInfo(hash, format, channels)
  }
  decodeTsv(tsv, idx2hash, decodeRow, ModInfo("", "", 0))
}

def encodeModInfosTsv(modinfos: Buffer[ModInfo], _idx: Map[String, String]) = {
  assert(modinfos.map(_.hash).distinct.size == modinfos.size)
  def sorter = (e: ModInfo) => e.format + SORT + e.channels + SORT + e.hash
  val infos = modinfos.sortBy(sorter).par.map(m =>
    Buffer(
      m.hash.take(12),
      m.format,
      if (m.channels > 0) m.channels.toString else ""
    )
  ).seq
  val dedupped = dedupidx(infos, "modinfos.tsv", _idx, strict=true)
  validate(dedupped, "modinfos.tsv")
  hashidxdiff(dedupped).map(_.mkString("\t").trim).mkString("\n").concat("\n")
}

def decodeMetaTsv(tsv: String, idx2hash: Buffer[String]) = {
  def decodeRow(cols: Array[String], hash: String, prev: MetaData) = {
    val authors = if (cols.length > 1) {
      if (cols(1) == REPEAT) prev.authors
      else if (cols(1).isEmpty) Buffer.empty
      else cols(1).split(SEPARATOR).toBuffer
    } else Buffer.empty
    
    val publishers = if (cols.length > 2) {
      if (cols(2) == REPEAT) prev.publishers
      else if (cols(2).isEmpty) Buffer.empty
      else cols(2).split(SEPARATOR).toBuffer
    } else Buffer.empty

    val album = if (cols.length > 3) {
      if (cols(3) == REPEAT) prev.album
      else cols(3)
    } else ""

    val year = if (cols.length > 4) {
      if (cols(4) == REPEAT) prev.year
      else cols(4).toInt
    } else 0

    MetaData(hash, authors, publishers, album, year)
  }
  decodeTsv(tsv, idx2hash, decodeRow, MetaData("", Buffer.empty, Buffer.empty, "", 0))
}

def encodeMetaTsv(meta: Buffer[MetaData], name: String, _idx: Map[String, String]) = {
  def sorter = (e: MetaData) =>
    e.authors.mkString(SEPARATOR) + SORT +
    e.publishers.mkString(SEPARATOR) + SORT +
    e.album + SORT + 
    e.year + SORT +
    e.hash
  val infos = meta.sortBy(sorter).par.map(i =>
    Buffer(
      i.hash.take(12),
      i.authors.mkString(SEPARATOR),
      i.publishers.mkString(SEPARATOR),
      i.album,
      if (i.year > 0) i.year.toString else ""
    )
  ).seq
  val dedupped = dedupidx(infos, name, _idx)
  validate(dedupped, name)
  hashidxdiff(dedupped).map(_.mkString("\t").trim).mkString("\n").concat("\n")
}
