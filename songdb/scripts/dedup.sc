// SPDX-License-Identifier: GPL-2.0-or-later AND CC-PDM-1.0
// SPDX-AI-Disclosure: ai-assisted
// Copyright (C) 2023-2026 Matti Tiainen <mvtiaine@cc.hut.fi>

import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.collection.parallel.CollectionConverters._

import md5._

val REPEAT = "\u007F"
val SORT = "\u0001"

def dedup(entries: Iterable[Buffer[String]], file: String, _check: Map[String,String]) = {
  import Ordering.Implicits._
  // keeps original order
  val keys = entries.map(_(0)).toSeq.distinct
  val dedupped = entries.groupBy(_(0)).par.map(e =>
    if (e._2.size > 1) {
      System.err.println(s"WARN: removing duplicate entries in ${file}, hash: ${_check(e._1)} entries: ${e._2}")
    }
    (e._1, e._2.toSeq.sorted.head)
  ).seq
  keys.map(dedupped).toSeq
}

def dedupidx(entries: Iterable[Buffer[String]], file: String, _idx: Map[String,String], strict: Boolean = false) = {
  // keeps original order
  val keys = entries.par.map(_(0)).seq.toSeq.distinct
  val groups = HashMap.empty[String, Buffer[Buffer[String]]]
  entries.foreach { e =>
    val k = e(0)
    val g = groups.get(k)
    if (g.isDefined) g.get += e else groups(k) = Buffer(e)
  }
  val dedupped = groups.par.map(e =>
    if (e._2.size > 1) {
      if (strict) {
        assert(e._2.forall(_ == e._2.head))
      } else {
        System.err.println(s"WARN: removing duplicate entries in ${file}, hash: ${e._1} entries: ${e._2}")
      }
    }
    val rows = e._2
    if (rows.size < 2) (e._1, rows.head)
    else {
      val keyed = new Array[(String, Buffer[String])](rows.size)
      var k = 0
      rows.foreach { r => keyed(k) = (r.tail.mkString(SORT), r); k += 1 }
      java.util.Arrays.sort(keyed, (a: (String, Buffer[String]), b: (String, Buffer[String])) => a._1.compareTo(b._1))
      (e._1, keyed(0)._2)
    }
  ).seq
  val res = Buffer.empty[Buffer[String]]
  var prev = Buffer.empty[String]
  for (k <- keys) {
    val s = dedupped(k)
    val idx = _idx(s.head)
    assert(base64d24(idx) > 0)
    val tail = s.tail
    if (tail.sameElements(prev)) {
      res += Buffer(idx)
    } else if (!prev.isEmpty && prev.length <= tail.length) {
      val tmp = Buffer.empty[String]
      for (i <- tail.indices) {
        if (i < prev.length && prev(i) == tail(i) && !tail(i).isEmpty) {
          tmp += REPEAT
        } else {
          tmp += tail(i)
        }
      }
      res += Buffer(idx) ++ tmp
    } else {
      res += Buffer(idx) ++ tail
    }
    prev = tail
  }
  res.toSeq
}

def validate(entries: Iterable[Buffer[String]], file: String) = {
  val check = entries.toSeq.par.map(_(0))
  if (check.size != check.distinct.size) {
    val dups = check.diff(check.distinct).distinct
    throw new IllegalStateException(s"Duplicate entries in ${file}: ${dups}")
  }
}
