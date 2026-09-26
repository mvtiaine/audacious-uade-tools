// SPDX-License-Identifier: CC-PDM-1.0
// SPDX-AI-Disclosure: ai-generated

// Lock-free open-addressing caches for the hot caches of the build (audio.sc
// `pairResults`). A `get` is one acquire read plus a linear
// probe over a long[]; a per-bucket tryLock wrapper costs several times the lookup it guards.
//
// DESIGN
//   * BUCKETED -- never a global lock. Sizing comes from the caller (PAIR_*).
//   * STAGGERED GROWTH: a bucket allocates on its first insert and doubles independently.
//     An untouched bucket costs 0.
//   * Linear probing; no deletions, so no tombstones.
//   * `0L` is the empty-key sentinel and index 0 is unused, so a key of exactly 0L is never
//     cached -- a permanent miss, never a wrong answer.
//
// MEMORY MODEL -- the contract: this cache may MISS, it must never be WRONG.
//   * A slot is CLAIMED with `keys.compareAndSet(i, 0L, key)`, the value stored AFTER the
//     claim, so only the winner writes that slot's value.
//   * A reader seeing the key before the value reads `absentValue` and reports a miss -- hence
//     `Arrays.fill(values, absentValue)` at allocation. All values are idempotent, and `put`
//     repairs a present-but-absent value so the window cannot become permanent.
//   * A grow publishes new arrays through ONE `tables.compareAndSet(b, old, new)`, so a reader
//     always sees a consistent (keys, values) pair and a stale grow cannot clobber a newer one.
//     Growers elect one winner via `growing`; `put` re-checks the published reference so an
//     entry written into a discarded table is retried.
//   * No tearing: `AtomicLongArray` and the `short`/`float` arrays are specified atomic.
//
// GROWTH TRIGGER -- 83 %, never 100 %. An unsuccessful linear probe costs
// 0.5*(1+1/(1-alpha)^2) searches, so a table ridden to alpha 1.0 is thousands of probes.
//
// NO CEILING -- a bucket doubles forever. Memory is already bounded by the trigger alone:
// len <= 2 * live / 0.83, so the footprint tracks live entries and cannot run away from the
// data. A ceiling added no protection, only two costs: it silently dropped entries once a
// bucket reached it, and a full bucket made every miss a full-table scan. Footprint is
// observable instead -- `onGrowBytes` publishes every allocation and `tailStats()` reports
// the bucket-length histogram.
//
// THE LIVE COUNT IS A PLAIN INT, AND IT IS ALLOWED TO BE WRONG. An atomic counter per insert
// hands the cache line to whoever inserted last; `@volatile` pays coherence and is still
// inaccurate. `size()` is approximate and diagnostics-only -- `put` grows anyway when the probe
// loop exhausts the table. Do not re-add: sampling the counter, or forcing a grow from a long
// probe (it fires exactly when the bucket is dense, discarding live inserters).

import java.util.concurrent.atomic.AtomicLongArray
import java.util.concurrent.atomic.AtomicReferenceArray
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.LongAdder

/** Slot count at which a table of `n` slots must grow: 83 %, never 100 %. */
private def growThreshold(n: Int): Int = { val g = (n.toLong * 83L / 100L).toInt; if (g < 1) 1 else g }

/**
 * Published snapshot of one bucket: two parallel arrays, the probe mask, and the mutable
 * bucket state -- `live` (claimed slots, the grow trigger) and `growing` (the
 * exactly-one-rebuilder flag). Arrays are never resized in place; a grow publishes a
 * whole new snapshot. `live` MUST live here and not in a map-wide array: a grow that
 * loses its CAS discards this snapshot along with the inserts that landed in it, and
 * their increments have to vanish with it.
 */
final class LFSTable(val keys: AtomicLongArray, val values: Array[Short], val growAt: Int) {
  val mask: Int = keys.length() - 1
  val length: Int = keys.length()
  /** Claimed slot count. Plain int, approximate under races, recounted at every grow. */
  var live: Int = 0
  val growing: AtomicBoolean = new AtomicBoolean(false)
}

final class LFFTableF(val keys: AtomicLongArray, val values: Array[Float], val growAt: Int) {
  val mask: Int = keys.length() - 1
  val length: Int = keys.length()
  var live: Int = 0
  val growing: AtomicBoolean = new AtomicBoolean(false)
}

final class LFBTable(val keys: AtomicLongArray, val values: Array[Byte], val growAt: Int) {
  val mask: Int = keys.length() - 1
  val length: Int = keys.length()
  var live: Int = 0
  val growing: AtomicBoolean = new AtomicBoolean(false)
}

/**
 * Lock-free concurrent long -> short map.
 *
 * @param numBuckets   number of independent buckets (fixed for the map's life)
 * @param initSlots    slot count of a bucket's FIRST allocation (power of two)
 * @param absentValue  the "not cached" sentinel; must never be a real value
 * @param onGrowBytes  called with the extra bytes of every allocation/grow, so the
 *                     caller can track the live footprint and its peak
 */
final class LockFreeLongShortMap(
    val numBuckets: Int,
    val initSlots: Int,
    val absentValue: Short,
    onGrowBytes: Long => Unit = _ => ()
) {
  val bytesPerSlot: Long = 10L

  private val tables = new AtomicReferenceArray[LFSTable](numBuckets)
  // Diagnostics only: touched by allocBucket/grow, never per insert.
  private val liveSlots = new LongAdder
  private val grownBuckets = new LongAdder
  private val allocatedBuckets = new LongAdder

  // A fold, not a finalizer: keys are hash-derived and already uniform in every bit, so
  // there is nothing to avalanche. Folding bits 32-63 down lets both the bucket index
  // (`% numBuckets`) and the probe mask (`h & mask`) see all 64 bits for one XOR.
  private inline def spread(k: Long): Int = (k ^ (k >>> 32)).toInt

  /**
   * Bucket index from a LOOP-INVARIANT `seed` rather than the key. A caller sweeping a batch
   * of keys derived from one constant value -- a pair cache's `lo` half across an inner loop --
   * passes that value here, so the whole batch lands in ONE bucket: the `tables` indirection is
   * hoisted out of the loop and the bucket array stays L2-resident for the batch instead of
   * being re-chosen, and re-fetched from DRAM, per key. The probe index still comes from the
   * full key, so a batch spreads over the whole bucket.
   * `seed` MUST be a pure function of the key, the same for every key of a batch AND the same
   * for the same key reached from any other batch, or the batch misses on read-back.
   */
  private inline def bucketOf(seed: Long): Int = (spread(seed) & 0x7fffffff) % numBuckets

  /** Returns `absentValue` when the key is not cached. Never blocks, never allocates. */
  def get(key: Long): Short = get(key, key)

  /** Batched probe: bucket from `seed`, slot from `key`. See `bucketOf`. */
  def get(key: Long, seed: Long): Short = {
    if (key == 0L) return absentValue
    val h = spread(key)
    val t = tables.getAcquire(bucketOf(seed))
    if (t == null) return absentValue
    val keys = t.keys
    val values = t.values
    val mask = t.mask
    var i = if ((h & mask) == 0) 1 else h & mask
    var probes = 1
    while (probes <= t.length) {
      val k = keys.getOpaque(i)
      if (k == key) return values(i)
      if (k == 0L) return absentValue
      i = (i + 1) & mask
      probes += 1
    }
    absentValue
  }

  /** Stores `value` under `key`. Idempotent: the same key always takes the same value. */
  def put(key: Long, value: Short): Unit = put(key, value, key)

  /** Batched store: bucket from `seed`, slot from `key`. `seed` must match the `get`. */
  def put(key: Long, value: Short, seed: Long): Unit = {
    if (key == 0L) return
    val h = spread(key)
    val b = bucketOf(seed)
    var done = false
    while (!done) {
      var t = tables.getAcquire(b)
      if (t == null) t = allocBucket(b, initSlots)
      val len = t.length
      // Plain read of a plain int: may be stale by a few, delaying the grow by a
      // handful of slots -- see the header.
      val lv = t.live
      if (lv >= t.growAt) {
        // Grow BEFORE the bucket fills -- see the header. If a rival owns the rebuild we
        // loop and re-read the reference. Inserting here instead would write into a table
        // that is about to be discarded, i.e. lose the entry.
        grow(b, t)
      } else {
        val keys = t.keys
        val values = t.values
        val mask = t.mask
        var i = if ((h & mask) == 0) 1 else h & mask
        var probes = 1
        var settled = false
        var claimed = false
        while (!settled && probes <= len) {
          val k = keys.getOpaque(i)
          if (k == key) {
            // Repair the claim-without-value window so a miss cannot become permanent.
            if (values(i) == absentValue) values(i) = value
            settled = true
          } else if (k == 0L) {
            if (keys.compareAndSet(i, 0L, key)) {
              values(i) = value
              claimed = true
              settled = true
            }
            // CAS lost: stay on `i` and re-read. Advancing would insert a duplicate of a
            // key already in the table -- a permanent capacity leak.
          }
          if (!settled && k != 0L) { i = (i + 1) & mask; probes += 1 }
        }
        if (settled) {
          // PLAIN, non-atomic, on purpose -- see the header.
          if (claimed) t.live += 1
          // A grow may have published a new table while we were inserting, discarding our
          // slot. Retry in that case.
          done = tables.getAcquire(b).`eq`(t)
        } else grow(b, t)
      }
    }
  }

  /**
   * Number of cached entries -- approximate (the per-bucket counters are plain ints), and
   * diagnostic only. Deliberately not a map-wide counter: one shared counter under
   * concurrent insert is a cache-line ping-pong on every put.
   */
  def size(): Long = {
    var s = 0L
    var i = 0
    while (i < numBuckets) {
      val t = tables.getAcquire(i)
      if (t != null) s += t.live.toLong
      i += 1
    }
    s
  }

  /** Allocated slot count across all buckets. */
  def usedSlots(): Long = liveSlots.sum()

  def allocatedBytes(): Long = liveSlots.sum() * bytesPerSlot

  def bucketCount(): Int = allocatedBuckets.sum().toInt

  def growCount(): Long = grownBuckets.sum()

  /**
   * Eviction-time distribution scan: (maxLive, maxLen, allocated, histogram).
   * `hist(k)` = allocated buckets whose length is 2^k. `size()`/`usedSlots()` report the MEAN;
   * the seed-batched caller concentrates a whole batch into ONE bucket, so the tail is where
   * the footprint and the probe cost actually live. O(numBuckets), never on a hot path.
   */
  def tailStats(): (Int, Int, Int, Array[Int]) = {
    var maxLive = 0
    var maxLen = 0
    var allocated = 0
    val hist = new Array[Int](31)
    var i = 0
    while (i < numBuckets) {
      val t = tables.getAcquire(i)
      if (t != null) {
        allocated += 1
        if (t.live > maxLive) maxLive = t.live
        if (t.length > maxLen) maxLen = t.length
        hist(31 - java.lang.Integer.numberOfLeadingZeros(t.length)) += 1
      }
      i += 1
    }
    (maxLive, maxLen, allocated, hist)
  }

  /** Per-bucket diagnostics for the sizing self-checks in bench-grow.sc. */
  def sizeOfBucket(b: Int): Int = {
    val t = tables.getAcquire(b)
    if (t == null) 0 else t.live
  }

  /**
   * TRUE live count of bucket `b`, scanned from the key array. Unlike `sizeOfBucket` this
   * is exact, so it is the one that can audit the grow trigger. O(slots), diagnostic.
   */
  def trueSizeOfBucket(b: Int): Int = {
    val t = tables.getAcquire(b)
    if (t == null) 0
    else {
      var n = 0
      var i = 0
      while (i < t.length) { if (t.keys.getOpaque(i) != 0L) n += 1; i += 1 }
      n
    }
  }

  def lengthOfBucket(b: Int): Int = {
    val t = tables.getAcquire(b)
    if (t == null) 0 else t.length
  }

  private def allocBucket(b: Int, n: Int): LFSTable = {
    val keys = new AtomicLongArray(n)
    val values = new Array[Short](n)
    java.util.Arrays.fill(values, absentValue)
    val t = new LFSTable(keys, values, growThreshold(n))
    val prev = tables.compareAndExchange(b, null, t)
    if (prev == null) {
      allocatedBuckets.increment()
      liveSlots.add(n)
      onGrowBytes(n * bytesPerSlot)
      t
    } else prev
  }

  /**
   * Rebuild bucket `b` at double the size. A rival owning the rebuild returns immediately,
   * leaving the caller on the current table.
   */
  private def grow(b: Int, old: LFSTable): Unit = {
    // Exactly one rebuilder per snapshot, flag living IN the snapshot -- no lock objects.
    if (!old.growing.compareAndSet(false, true)) return
    val n = old.length << 1
    val keys = new AtomicLongArray(n)
    val values = new Array[Short](n)
    java.util.Arrays.fill(values, absentValue)
    val mask = n - 1
    var moved = 0
    var i = 0
    val oldLen = old.length
    while (i < oldLen) {
      val k = old.keys.getOpaque(i)
      if (k != 0L) {
        val v = old.values(i)
        // Skip a slot whose value is not stored yet -- the owner's put re-derives it.
        if (v != absentValue) {
          var j = spread(k) & mask
          if (j == 0) j = 1
          while (keys.getOpaque(j) != 0L) j = (j + 1) & mask
          keys.setOpaque(j, k)
          values(j) = v
          moved += 1
        }
      }
      i += 1
    }
    val nt = new LFSTable(keys, values, growThreshold(n))
    // Recounted exactly at every publish, bounding the lossy counter's error to one
    // table's lifetime.
    nt.live = moved
    // CAS, not set: discard ours rather than clobber a newer snapshot.
    if (tables.compareAndSet(b, old, nt)) {
      liveSlots.add(n.toLong - oldLen.toLong)
      grownBuckets.increment()
      onGrowBytes((n - oldLen) * bytesPerSlot)
    }
  }
}

/**
 * Lock-free concurrent long -> float map. Same protocol as [[LockFreeLongShortMap]];
 * separate class because Scala generics would box the primitive values.
 */
final class LockFreeLongFloatMap(
    val numBuckets: Int,
    val initSlots: Int,
    val absentValue: Float,
    onGrowBytes: Long => Unit = _ => ()
) {
  val bytesPerSlot: Long = 12L

  private val tables = new AtomicReferenceArray[LFFTableF](numBuckets)
  private val liveSlots = new LongAdder
  private val grownBuckets = new LongAdder
  private val allocatedBuckets = new LongAdder

  // See the comment on LockFreeLongShortMap.spread -- same fold, same reason.
  private inline def spread(k: Long): Int = (k ^ (k >>> 32)).toInt

  def get(key: Long): Float = {
    if (key == 0L) return absentValue
    val h = spread(key)
    val t = tables.getAcquire((h & 0x7fffffff) % numBuckets)
    if (t == null) return absentValue
    val keys = t.keys
    val values = t.values
    val mask = t.mask
    var i = if ((h & mask) == 0) 1 else h & mask
    var probes = 1
    while (probes <= t.length) {
      val k = keys.getOpaque(i)
      if (k == key) return values(i)
      if (k == 0L) return absentValue
      i = (i + 1) & mask
      probes += 1
    }
    absentValue
  }

  def put(key: Long, value: Float): Unit = {
    if (key == 0L) return
    val h = spread(key)
    val b = (h & 0x7fffffff) % numBuckets
    var done = false
    while (!done) {
      var t = tables.getAcquire(b)
      if (t == null) t = allocBucket(b, initSlots)
      val len = t.length
      // Grow BEFORE the bucket fills -- see the long/short variant above.
      val lv = t.live
      if (lv >= t.growAt) grow(b, t)
      else {
        val keys = t.keys
        val values = t.values
        val mask = t.mask
        var i = if ((h & mask) == 0) 1 else h & mask
        var probes = 1
        var settled = false
        var claimed = false
        while (!settled && probes <= len) {
          val k = keys.getOpaque(i)
          if (k == key) {
            if (values(i) == absentValue) values(i) = value
            settled = true
          } else if (k == 0L) {
            if (keys.compareAndSet(i, 0L, key)) {
              values(i) = value
              claimed = true
              settled = true
            }
            // CAS lost: stay on `i` and re-read, see the long/short variant above.
          }
          if (!settled && k != 0L) { i = (i + 1) & mask; probes += 1 }
        }
        if (settled) {
          // Plain, non-atomic, on purpose -- see the long/short variant above.
          if (claimed) t.live += 1
          // Retry if a grow discarded our slot -- see the long/short variant above.
          done = tables.getAcquire(b).`eq`(t)
        } else grow(b, t)
      }
    }
  }

  def size(): Long = {
    var s = 0L
    var i = 0
    while (i < numBuckets) {
      val t = tables.getAcquire(i)
      if (t != null) s += t.live.toLong
      i += 1
    }
    s
  }

  // See the comment on LockFreeLongShortMap.tailStats.
  def tailStats(): (Int, Int, Int, Array[Int]) = {
    var maxLive = 0
    var maxLen = 0
    var allocated = 0
    val hist = new Array[Int](31)
    var i = 0
    while (i < numBuckets) {
      val t = tables.getAcquire(i)
      if (t != null) {
        allocated += 1
        if (t.live > maxLive) maxLive = t.live
        if (t.length > maxLen) maxLen = t.length
        hist(31 - java.lang.Integer.numberOfLeadingZeros(t.length)) += 1
      }
      i += 1
    }
    (maxLive, maxLen, allocated, hist)
  }

  def sizeOfBucket(b: Int): Int = {
    val t = tables.getAcquire(b)
    if (t == null) 0 else t.live
  }

  def trueSizeOfBucket(b: Int): Int = {
    val t = tables.getAcquire(b)
    if (t == null) 0
    else {
      var n = 0
      var i = 0
      while (i < t.length) { if (t.keys.getOpaque(i) != 0L) n += 1; i += 1 }
      n
    }
  }

  def lengthOfBucket(b: Int): Int = {
    val t = tables.getAcquire(b)
    if (t == null) 0 else t.length
  }

  def usedSlots(): Long = liveSlots.sum()

  def allocatedBytes(): Long = liveSlots.sum() * bytesPerSlot

  def bucketCount(): Int = allocatedBuckets.sum().toInt

  def growCount(): Long = grownBuckets.sum()

  private def allocBucket(b: Int, n: Int): LFFTableF = {
    val keys = new AtomicLongArray(n)
    val values = new Array[Float](n)
    java.util.Arrays.fill(values, absentValue)
    val t = new LFFTableF(keys, values, growThreshold(n))
    val prev = tables.compareAndExchange(b, null, t)
    if (prev == null) {
      allocatedBuckets.increment()
      liveSlots.add(n)
      onGrowBytes(n * bytesPerSlot)
      t
    } else prev
  }

  private def grow(b: Int, old: LFFTableF): Unit = {
    if (!old.growing.compareAndSet(false, true)) return
    val n = old.length << 1
    val keys = new AtomicLongArray(n)
    val values = new Array[Float](n)
    java.util.Arrays.fill(values, absentValue)
    val mask = n - 1
    var moved = 0
    var i = 0
    val oldLen = old.length
    while (i < oldLen) {
      val k = old.keys.getOpaque(i)
      if (k != 0L) {
        val v = old.values(i)
        if (v != absentValue) {
          var j = spread(k) & mask
          if (j == 0) j = 1
          while (keys.getOpaque(j) != 0L) j = (j + 1) & mask
          keys.setOpaque(j, k)
          values(j) = v
          moved += 1
        }
      }
      i += 1
    }
    val nt = new LFFTableF(keys, values, growThreshold(n))
    // Recounted exactly at every publish point -- bounds the lossy counter's error.
    nt.live = moved
    if (tables.compareAndSet(b, old, nt)) {
      liveSlots.add(n.toLong - oldLen.toLong)
      grownBuckets.increment()
      onGrowBytes((n - oldLen) * bytesPerSlot)
    }
  }
}

/**
 * Lock-free concurrent long -> byte map. Same protocol as [[LockFreeLongShortMap]];
 * separate class because Scala generics would box the primitive values. Used by the
 * audio.sc `pairResults` cache (see `pairDuplicateResult` for the bit packing);
 * `Byte.MinValue` is the "absent" sentinel there.
 */
final class LockFreeLongByteMap(
    val numBuckets: Int,
    val initSlots: Int,
    val absentValue: Byte,
    onGrowBytes: Long => Unit = _ => ()
) {
  val bytesPerSlot: Long = 9L

  private val tables = new AtomicReferenceArray[LFBTable](numBuckets)
  private val liveSlots = new LongAdder
  private val grownBuckets = new LongAdder
  private val allocatedBuckets = new LongAdder

  // See the comment on LockFreeLongShortMap.spread -- same fold, same reason.
  private inline def spread(k: Long): Int = (k ^ (k >>> 32)).toInt

  // See the comment on LockFreeLongShortMap.bucketOf -- same seed contract, same reason.
  private inline def bucketOf(seed: Long): Int = (spread(seed) & 0x7fffffff) % numBuckets

  def get(key: Long): Byte = get(key, key)

  /** Batched probe: bucket from `seed`, slot from `key`. */
  def get(key: Long, seed: Long): Byte = {
    if (key == 0L) return absentValue
    val h = spread(key)
    val t = tables.getAcquire(bucketOf(seed))
    if (t == null) return absentValue
    val keys = t.keys
    val values = t.values
    val mask = t.mask
    var i = if ((h & mask) == 0) 1 else h & mask
    var probes = 1
    while (probes <= t.length) {
      val k = keys.getOpaque(i)
      if (k == key) return values(i)
      if (k == 0L) return absentValue
      i = (i + 1) & mask
      probes += 1
    }
    absentValue
  }

  def put(key: Long, value: Byte): Unit = put(key, value, key)

  /** Batched store: bucket from `seed`, slot from `key`. `seed` must match the `get`. */
  def put(key: Long, value: Byte, seed: Long): Unit = {
    if (key == 0L) return
    val h = spread(key)
    val b = bucketOf(seed)
    var done = false
    while (!done) {
      var t = tables.getAcquire(b)
      if (t == null) t = allocBucket(b, initSlots)
      val len = t.length
      // Grow BEFORE the bucket fills -- see the long/short variant above.
      val lv = t.live
      if (lv >= t.growAt) grow(b, t)
      else {
        val keys = t.keys
        val values = t.values
        val mask = t.mask
        var i = if ((h & mask) == 0) 1 else h & mask
        var probes = 1
        var settled = false
        var claimed = false
        while (!settled && probes <= len) {
          val k = keys.getOpaque(i)
          if (k == key) {
            if (values(i) == absentValue) values(i) = value
            settled = true
          } else if (k == 0L) {
            if (keys.compareAndSet(i, 0L, key)) {
              values(i) = value
              claimed = true
              settled = true
            }
            // CAS lost: stay on `i` and re-read, see the long/short variant above.
          }
          if (!settled && k != 0L) { i = (i + 1) & mask; probes += 1 }
        }
        if (settled) {
          // Plain, non-atomic, on purpose -- see the long/short variant above.
          if (claimed) t.live += 1
          // Retry if a grow discarded our slot -- see the long/short variant above.
          done = tables.getAcquire(b).`eq`(t)
        } else grow(b, t)
      }
    }
  }

  def size(): Long = {
    var s = 0L
    var i = 0
    while (i < numBuckets) {
      val t = tables.getAcquire(i)
      if (t != null) s += t.live.toLong
      i += 1
    }
    s
  }

  def sizeOfBucket(b: Int): Int = {
    val t = tables.getAcquire(b)
    if (t == null) 0 else t.live
  }

  def trueSizeOfBucket(b: Int): Int = {
    val t = tables.getAcquire(b)
    if (t == null) 0
    else {
      var n = 0
      var i = 0
      while (i < t.length) { if (t.keys.getOpaque(i) != 0L) n += 1; i += 1 }
      n
    }
  }

  def lengthOfBucket(b: Int): Int = {
    val t = tables.getAcquire(b)
    if (t == null) 0 else t.length
  }

  def usedSlots(): Long = liveSlots.sum()

  def allocatedBytes(): Long = liveSlots.sum() * bytesPerSlot

  def bucketCount(): Int = allocatedBuckets.sum().toInt

  def growCount(): Long = grownBuckets.sum()

  /**
   * Eviction-time distribution scan: (maxLive, maxLen, allocated, histogram).
   * `hist(k)` = allocated buckets whose length is 2^k. `size()`/`usedSlots()` report the MEAN;
   * the seed-batched caller concentrates a whole batch into ONE bucket, so the tail is where
   * the footprint and the probe cost actually live. O(numBuckets), never on a hot path.
   */
  def tailStats(): (Int, Int, Int, Array[Int]) = {
    var maxLive = 0
    var maxLen = 0
    var allocated = 0
    val hist = new Array[Int](31)
    var i = 0
    while (i < numBuckets) {
      val t = tables.getAcquire(i)
      if (t != null) {
        allocated += 1
        if (t.live > maxLive) maxLive = t.live
        if (t.length > maxLen) maxLen = t.length
        hist(31 - java.lang.Integer.numberOfLeadingZeros(t.length)) += 1
      }
      i += 1
    }
    (maxLive, maxLen, allocated, hist)
  }

  private def allocBucket(b: Int, n: Int): LFBTable = {
    val keys = new AtomicLongArray(n)
    val values = new Array[Byte](n)
    java.util.Arrays.fill(values, absentValue)
    val t = new LFBTable(keys, values, growThreshold(n))
    val prev = tables.compareAndExchange(b, null, t)
    if (prev == null) {
      allocatedBuckets.increment()
      liveSlots.add(n)
      onGrowBytes(n * bytesPerSlot)
      t
    } else prev
  }

  private def grow(b: Int, old: LFBTable): Unit = {
    if (!old.growing.compareAndSet(false, true)) return
    val n = old.length << 1
    val keys = new AtomicLongArray(n)
    val values = new Array[Byte](n)
    java.util.Arrays.fill(values, absentValue)
    val mask = n - 1
    var moved = 0
    var i = 0
    val oldLen = old.length
    while (i < oldLen) {
      val k = old.keys.getOpaque(i)
      if (k != 0L) {
        val v = old.values(i)
        if (v != absentValue) {
          var j = spread(k) & mask
          if (j == 0) j = 1
          while (keys.getOpaque(j) != 0L) j = (j + 1) & mask
          keys.setOpaque(j, k)
          values(j) = v
          moved += 1
        }
      }
      i += 1
    }
    val nt = new LFBTable(keys, values, growThreshold(n))
    // Recounted exactly at every publish point -- bounds the lossy counter's error.
    nt.live = moved
    if (tables.compareAndSet(b, old, nt)) {
      liveSlots.add(n.toLong - oldLen.toLong)
      grownBuckets.increment()
      onGrowBytes((n - oldLen) * bytesPerSlot)
    }
  }
}
