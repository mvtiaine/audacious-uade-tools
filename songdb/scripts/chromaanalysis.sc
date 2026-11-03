// SPDX-License-Identifier: CC-PDM-1.0
// SPDX-AI-Disclosure: ai-generated

import java.util.Arrays

// number of 32-bit words per silence/content scan window
val chunkSize = 16

// Silence scan; early exits keep it free for real content. Do not merge into the full-scan analyses below.
def isSilentChunk(data: Array[Int], len: Int, offset: Int = 0): Boolean = {
  if (len <= 0) return true
  val zeroTrue  = (9 * len) / 10          // zeroCount  >  zeroTrue  <=> zeroRatio   >  0.9
  val bitsDead1 = (32 * len + 199) / 200 // totalBits >= bitsDead1 <=> setBitRatio  >= 0.005
  val bitsDead3 = (32 * len + 49) / 50   // totalBits >= bitsDead3 <=> setBitRatio  >= 0.02
  val uniqDead  = (len + 19) / 20        // uniq      >= uniqDead  <=> repetitionRatio >= 0.05
  var totalBits = 0
  var zeroCount = 0
  // the first four distinct values seen; uniq saturates at 4 == ">= 4"
  var u0 = 0
  var u1 = 0
  var u2 = 0
  var uniq = 0
  var i = 0
  while (i < len) {
    val v = data(offset + i)
    totalBits += Integer.bitCount(v)
    if (v == 0) zeroCount += 1
    if (uniq == 0) { u0 = v; uniq = 1 }
    else if (uniq == 1) { if (v != u0) { u1 = v; uniq = 2 } }
    else if (uniq == 2) { if (v != u0 && v != u1) { u2 = v; uniq = 3 } }
    else if (uniq == 3) { if (v != u0 && v != u1 && v != u2) uniq = 4 }
    if (zeroCount > zeroTrue) return true
    if (totalBits >= bitsDead1 && uniq >= 3 && (totalBits >= bitsDead3 || uniq >= uniqDead)
        && zeroCount + len - i - 1 <= zeroTrue) return false
    i += 1
  }
  if (totalBits < bitsDead1) return true
  if (uniq <= 2) return true
  if (totalBits < bitsDead3 && uniq < uniqDead) return true
  false
}

// Bad/good evidence scored separately, totals compared (isBadFingerprint); rejected only
// when bad outweighs good. Points are relative weights.
// Bad evidence. Silence alone is conclusive.
val badRepetitivePoints = 75
val badNoisyPoints = 60
// stuck tone: max bit positions ever changing in the non-zero words
val stuckVariedMax = 2
// min length before repetition counts as evidence (~25 s at ~7.9 words/s)
val repetitiveMinWords = 200
// below this share out of 100 audible words the rendering is mostly silence
val repetitiveMinLiveRatio = 15
// repeated-word share out of 10 words, see isRepetitiveFingerprint. Needs enough
// occurrences on a short-enough fingerprint (a long module may repeat a phrase).
val repetitiveWordRatio = 5
val repetitiveWordMinWords = 60
// above this length the ratio alone does not decide; drift must confirm:
// max share out of 100 consecutive word pairs differing by <= 1 bit
val repetitiveWordMaxWords = 550
val repetitiveLongDriftRatio = 68
// min share out of 100 audible words sitting in runs of >= 4 identical words
val repetitiveLiveRepeatRatio = 15
// saturated noise: mean bits out of 32 (real content ~16.4) and max flickering bit
// positions. Global, not chunk-local.
val noisyMeanBits = 24
val noisyVariedPosMax = 12
// slow-drift noise (crowd noise, rain): meanBits gate out of 10, drift share out of 100
// consecutive word pairs. Drum-heavy techno has low variedPos but lower drift, so the
// lower meanBits gate needs the higher crowd ratio.
val noisyDriftMeanBits = 225
val noisyDriftRatio = 35
val noisyCrowdDriftRatio = 55
// saturated sweep: new words keep appearing, nothing restated from further back than
// noisyRampBackWords. Out of 100 audible words: min first occurrences, max long-distance
// repeats.
val noisyRampBackWords = 32
val noisyRampNewMin = 60
val noisyRampBackMax = 16
// min audible words before novelty proves absence of structure
val noisyRampMinWords = 320
// min length before noise is judged; below repetitiveMinWords (saturation shows early)
val noisyMinWords = 150

// Unusable: too monotonic to identify a tune (drum loop, drone, SFX). Gets its audioHash
// tag but not the length bucket tag. Gates on audible words: min length, max distinct,
// max entropy.
val unusableMinLiveWords = 150
val unusableMaxDistinct = 300
val unusableMaxEntropy = 6.0

// Good evidence. Matching the bad total clears the fingerprint.
// Phrase repetition: best period >= goodPhraseMinPeriod words (~4 s, capped so the scan
// stays linear), min share of equal word pairs out of 100, repeating unit must be varied.
val goodPhraseMinWords = 200
val goodPhraseMinPeriod = 32
val goodPhraseMaxPeriod = 400
val goodPhraseRatio = 20
val goodPhraseUnit = 12
val goodPhrasePoints = 60
// varied audible content, out of 100 audible words: min distinct, max single-word run;
// plus min audible share of the fingerprint
val goodVariedAudibleRatio = 55
val goodVariedAudibleRunMax = 25
val goodVariedAudibleLiveMin = 20
val goodVariedAudiblePoints = 40
// long varied audible part outweighs a frozen quiet intro; mean bits out of 320 over
// audible words gates out saturated noise (real ~165, noise 240+)
val goodSustainedAudibleWords = 300
val goodSustainedAudibleRatio = 55
val goodSustainedAudibleMaxBits = 200
val goodSustainedAudiblePoints = 60
// min bit positions flickering in 20-80 % of frames (saturation collapses them)
val goodFlickerPositions = 15
val goodFlickerPoints = 30
// max share out of 100 consecutive word pairs differing by <= 1 bit
val goodDriftMax = 30
val goodDriftPoints = 25
// out of 320, same guard as goodSustainedAudibleMaxBits: above this, drift/phrase
// evidence does not count (saturated)
val goodSaturatedMaxBits = 235

// the words of non-silent chunks
def audibleWords(data: Array[Int]): Array[Int] = {
  val n = data.length
  val numChunks = (n + chunkSize - 1) / chunkSize
  var lvn = 0
  var c = 0
  while (c < numChunks) {
    val st = c * chunkSize
    if (!isSilentChunk(data, math.min(chunkSize, n - st), st)) lvn += math.min(chunkSize, n - st)
    c += 1
  }
  val lv = new Array[Int](lvn)
  var k = 0
  c = 0
  while (c < numChunks) {
    val st = c * chunkSize
    val len = math.min(chunkSize, n - st)
    if (!isSilentChunk(data, len, st)) { System.arraycopy(data, st, lv, k, len); k += len }
    c += 1
  }
  lv
}

def isSilentFingerprint(data: Array[Int]): Boolean = {
  if (data.isEmpty) return true
  var start = 0
  while (start < data.length) {
    val len = math.min(chunkSize, data.length - start)
    if (!isSilentChunk(data, len, start)) return false
    start += chunkSize
  }
  true
}

// Stuck tone or one word repeated. Whole-block, not chunked — chunk-silence exclusion
// would throw away the evidence. Only exact zeros dropped. Runs count from 4 (two
// repeats are normal in music). Modal count via sort — a HashMap per word is slower.
def isRepetitiveFingerprint(data: Array[Int]): Boolean = {
  val n = data.length
  if (n < repetitiveMinWords) return false
  val live = new Array[Int](n)
  var ln = 0
  var inRun = 0
  var run = 0
  var prev = 0
  var andAll = -1
  var orAll = 0
  var i = 0
  while (i < n) {
    val v = data(i)
    if (v != 0) {
      live(ln) = v; ln += 1
      andAll &= v
      orAll |= v
      if (v == prev) run += 1
      else { if (run >= 4) inRun += run; run = 1; prev = v }
    }
    i += 1
  }
  if (run >= 4) inRun += run
  if (ln < chunkSize) return false
  // Words in non-silent chunks; too little audible audio and nothing here is evidence.
  // Judged before the stuck-tone check, which a silent freeze trips just as easily.
  val audible = {
    val numChunks = (n + chunkSize - 1) / chunkSize
    var lvn = 0
    var c = 0
    while (c < numChunks) {
      val st = c * chunkSize
      if (!isSilentChunk(data, math.min(chunkSize, n - st), st)) lvn += math.min(chunkSize, n - st)
      c += 1
    }
    if (lvn * 100 < n * repetitiveMinLiveRatio) null
    else {
      val lv = new Array[Int](lvn)
      var k = 0
      c = 0
      while (c < numChunks) {
        val st = c * chunkSize
        val len = math.min(chunkSize, n - st)
        if (!isSilentChunk(data, len, st)) { System.arraycopy(data, st, lv, k, len); k += len }
        c += 1
      }
      lv
    }
  }
  if (audible == null) return false
  if (Integer.bitCount(orAll ^ andAll) <= stuckVariedMax) return true
  // share out of 100 audible words in runs of >= 4 identical words >= repetitiveLiveRepeatRatio
  def audibleRepeats(): Boolean = {
    val lv = audible
    val cnt = lv.length
    Arrays.sort(lv)
    var rep = 0
    var k = 0
    while (k < cnt) {
      var j = k
      while (j < cnt && lv(j) == lv(k)) j += 1
      if (j - k >= 4) rep += j - k
      k = j
    }
    rep * 100 >= cnt * repetitiveLiveRepeatRatio
  }
  val long = ln > repetitiveWordMaxWords
  if (!long && inRun >= repetitiveWordMinWords && inRun * 10 >= ln * repetitiveWordRatio && audibleRepeats()) return true
  Arrays.sort(live, 0, ln)
  var modal = 1
  var runLen = 1
  i = 1
  while (i < ln) {
    if (live(i) == live(i - 1)) { runLen += 1; if (runLen > modal) modal = runLen }
    else runLen = 1
    i += 1
  }
  if (modal * 10 < ln * repetitiveWordRatio) return false
  if (long) {
    var drift = 0
    i = 1
    while (i < n) {
      if (Integer.bitCount(data(i) ^ data(i - 1)) <= 1) drift += 1
      i += 1
    }
    drift * 100 >= (n - 1) * repetitiveLongDriftRatio && audibleRepeats()
  } else if (modal >= repetitiveWordMinWords) audibleRepeats()
  else false
}

// Saturated white noise, or slow drift at lower meanBits (crowd noise etc.) — no musical
// identity. Global, not chunk-local. meanBits gate first so the histogram is rare.
def isNoisyFingerprint(data: Array[Int]): Boolean = {
  val n = data.length
  if (n < noisyMinWords) return false
  var totalBits = 0L
  var i = 0
  while (i < n) { totalBits += Integer.bitCount(data(i)); i += 1 }
  if (totalBits * 10 < n.toLong * noisyDriftMeanBits) return false
  // Saturated sweep: nearly every audible word new, repeats only stutters of the
  // previous word. Checked before the variedPositions gate, which a wide sweep exceeds.
  val lv = audibleWords(data)
  val ln = lv.length
  if (ln >= noisyRampMinWords) {
    val mask = (Integer.highestOneBit(ln) << 2) - 1
    val keys = new Array[Int](mask + 1)
    val firsts = new Array[Int](mask + 1)
    val used = new Array[Int](mask + 1)
    var fresh = 0
    var restated = 0
    var k = 0
    var ramp = true
    while (k < ln && ramp) {
      val v = lv(k)
      var s = ((v ^ (v >>> 16)) * 0x7feb352d) & mask
      while (used(s) != 0 && keys(s) != v) s = (s + 1) & mask
      if (used(s) == 0) {
        used(s) = 1
        keys(s) = v
        firsts(s) = k
        fresh += 1
      } else if (k - firsts(s) > noisyRampBackWords) restated += 1
      if (fresh * 100 < (k + 1) * noisyRampNewMin) ramp = false
      if (restated * 100 > (k + 1) * noisyRampBackMax) ramp = false
      k += 1
    }
    if (ramp) return true
  }
  val pos = new Array[Int](32)
  i = 0
  while (i < n) {
    var v = data(i)
    while (v != 0) {
      pos(Integer.numberOfTrailingZeros(v)) += 1
      v &= v - 1
    }
    i += 1
  }
  var varied = 0
  var b = 0
  while (b < 32) {
    val f = pos(b)
    if (f * 5 > n && f * 5 < n * 4) varied += 1 // 0.2 < ratio < 0.8
    b += 1
  }
  if (varied > noisyVariedPosMax) return false
  // saturation alone is not noise: dense repeating structure has few distinct words
  // but does not drift; noise has many distinct words or drifts.
  if (totalBits >= noisyMeanBits.toLong * n) {
    val sorted = data.clone()
    Arrays.sort(sorted)
    var distinct = 1
    var j = 1
    while (j < n) {
      if (sorted(j) != sorted(j - 1)) distinct += 1
      j += 1
    }
    if (distinct * 2 >= n) return true
  }
  val neededDrift =
    if (totalBits >= noisyMeanBits.toLong * n - n) noisyDriftRatio else noisyCrowdDriftRatio
  var drift = 0
  i = 1
  while (i < n) {
    if (Integer.bitCount(data(i) ^ data(i - 1)) <= 1) drift += 1
    i += 1
  }
  drift * 100 >= (n - 1) * neededDrift
}

// Too monotonic to identify a tune. Distinct-word count in a fixed table, giving up as
// soon as it exceeds unusableMaxDistinct; then entropy over the word distribution.
def isUnusableFingerprint(data: Array[Int]): Boolean = {
  val lv = audibleWords(data)
  val n = lv.length
  if (n < unusableMinLiveWords) return false
  val mask = 1023
  val keys = new Array[Int](mask + 1)
  val cnts = new Array[Int](mask + 1)
  var distinct = 0
  var i = 0
  var overflow = false
  while (i < n && !overflow) {
    val v = lv(i)
    var s = ((v ^ (v >>> 16)) * 0x7feb352d) & mask
    while (cnts(s) != 0 && keys(s) != v) s = (s + 1) & mask
    if (cnts(s) == 0) {
      keys(s) = v
      cnts(s) = 1
      distinct += 1
      if (distinct > unusableMaxDistinct) overflow = true
    } else cnts(s) += 1
    i += 1
  }
  if (overflow) return false
  var entropy = 0.0
  var s = 0
  while (s <= mask) {
    val c = cnts(s)
    if (c > 0) {
      val p = c.toDouble / n
      entropy -= p * (math.log(p) / math.log(2))
    }
    s += 1
  }
  entropy < unusableMaxEntropy
}

// Musical structure a broken replay/stuck tone/noise cannot produce. Only called once
// some bad signal fired — the autocorrelation is O(n * periods).
def goodFingerprintPoints(data: Array[Int]): Int = {
  val n = data.length
  if (n == 0) return 0
  var points = 0
  // audible words, and how much of the fingerprint is audible at all
  val numChunks = (n + chunkSize - 1) / chunkSize
  var lvn = 0
  var c = 0
  while (c < numChunks) {
    val st = c * chunkSize
    if (!isSilentChunk(data, math.min(chunkSize, n - st), st)) lvn += math.min(chunkSize, n - st)
    c += 1
  }
  val audible = new Array[Int](lvn)
  var k = 0
  c = 0
  while (c < numChunks) {
    val st = c * chunkSize
    val len = math.min(chunkSize, n - st)
    if (!isSilentChunk(data, len, st)) { System.arraycopy(data, st, audible, k, len); k += len }
    c += 1
  }
  var bitsAud = 0L
  // varied audible content: many distinct words, no long run of one word
  if (lvn > 0) {
    Arrays.sort(audible)
    var distinct = 1
    var run = 1
    var maxRun = 1
    var i = 0
    while (i < lvn) {
      bitsAud += Integer.bitCount(audible(i))
      if (i > 0) {
        if (audible(i) == audible(i - 1)) { run += 1; if (run > maxRun) maxRun = run }
        else { distinct += 1; run = 1 }
      }
      i += 1
    }
    if (
      lvn * 100 >= n * goodVariedAudibleLiveMin &&
      distinct * 100 >= lvn * goodVariedAudibleRatio &&
      maxRun * 100 <= lvn * goodVariedAudibleRunMax
    ) points += goodVariedAudiblePoints
    if (
      lvn >= goodSustainedAudibleWords &&
      distinct * 100 >= lvn * goodSustainedAudibleRatio &&
      bitsAud * 10 <= lvn.toLong * goodSustainedAudibleMaxBits
    ) points += goodSustainedAudiblePoints
  }
  // bit positions that flicker between 20% and 80% of frames, and consecutive words that
  // develop rather than drift
  val pos = new Array[Int](32)
  var drift = 0
  var i = 0
  while (i < n) {
    var v = data(i)
    while (v != 0) { pos(Integer.numberOfTrailingZeros(v)) += 1; v &= v - 1 }
    if (i > 0 && Integer.bitCount(data(i) ^ data(i - 1)) <= 1) drift += 1
    i += 1
  }
  var varied = 0
  var b = 0
  while (b < 32) {
    val f = pos(b)
    if (f * 5 > n && f * 5 < n * 4) varied += 1
    b += 1
  }
  val saturated = bitsAud * 10 > lvn.toLong * goodSaturatedMaxBits
  if (varied >= goodFlickerPositions) points += goodFlickerPoints
  if (drift * 100 <= (n - 1) * goodDriftMax && !saturated) points += goodDriftPoints
  // phrase repetition: at the best period of at least goodPhraseMinPeriod words, how
  // many word pairs are equal, and how varied the repeating unit is
  if (n >= goodPhraseMinWords) {
    val maxP = math.min(n / 2, goodPhraseMaxPeriod)
    var bestRatio = 0.0
    var bestPeriod = 0
    var p = goodPhraseMinPeriod
    while (p <= maxP) {
      var eq = 0
      var i = 0
      while (i + p < n) { if (data(i) == data(i + p)) eq += 1; i += 1 }
      val ratio = eq.toDouble / (n - p)
      if (ratio > bestRatio) { bestRatio = ratio; bestPeriod = p }
      p += 1
    }
    if (bestRatio * 100 >= goodPhraseRatio && bestPeriod > 0) {
      val seen = new java.util.HashSet[Integer](bestPeriod)
      var i = 0
      while (i < bestPeriod) { seen.add(data(i)); i += 1 }
      if (seen.size >= goodPhraseUnit && !saturated) points += goodPhrasePoints
    }
  }
  points
}

// Silence alone is conclusive; repetition/noise must outweigh goodFingerprintPoints.
def isBadFingerprint(data: Array[Int]): Boolean = {
  if (isSilentFingerprint(data)) return true
  var bad = 0
  if (isRepetitiveFingerprint(data)) bad += badRepetitivePoints
  if (isNoisyFingerprint(data)) bad += badNoisyPoints
  bad > 0 && bad > goodFingerprintPoints(data)
}
