// SPDX-License-Identifier: CC-PDM-1.0
// SPDX-AI-Disclosure: ai-generated

// JFR phase markers. Committed events make phase boundaries facts in the recording; boundaries
// mined afterwards out of ExecutionSample stacks drift between runs on an identical dataset.
//
// PLATFORM NOTES (macOS arm64, Temurin JDK 25)
//   - No built-in jdk.Marker here, and profile.jfc does not record it -- hence a custom
//     jdk.jfr.Event subclass.
//   - Fields must be mutable `var`s on a plain class; JFR reflects over the class and a case
//     class / val-only field commits no readable values.
//   - @StackTrace(false): a marker is a timestamp, deep stacks are noise.
//   - jdk.CPUTimeSample emits nothing on macOS, so blocked threads are only visible through
//     ThreadDump plus these markers.
//
//   Jfr.mark("combineStart");  Jfr.mark("pass", pass);  Jfr.block("processAudioTags") { ... }
//   jfr print --events songdb.Phase,songdb.PhaseBlock <recording>.jfr

import jdk.jfr.Category
import jdk.jfr.Event
import jdk.jfr.Label
import jdk.jfr.Name
import jdk.jfr.StackTrace

@Name("songdb.Phase")
@Label("songdb phase")
@Category(Array("Songdb", "Phase"))
@StackTrace(false)
class PhaseEvent extends Event:
  @Label("phase") var phase: String = ""
  @Label("pass") var pass: Int = 0

@Name("songdb.PhaseBlock")
@Label("songdb phase block")
@Category(Array("Songdb", "Phase"))
@StackTrace(false)
class PhaseBlockEvent extends Event:
  @Label("phase") var phase: String = ""
  @Label("pass") var pass: Int = 0

object Jfr:

  /** Instantaneous boundary. `pass` 0 means "not pass-scoped". */
  def mark(phase: String): Unit = mark(phase, 0)

  def mark(phase: String, pass: Int): Unit =
    val e = PhaseEvent()
    e.phase = phase
    e.pass = pass
    e.commit()

  /**
   * Bracketed region: JFR records the duration itself, so nested or overlapping work
   * becomes measurable without diffing two markers by hand. The event is committed in
   * `finally` so a throwing body still produces a boundary instead of vanishing.
   */
  def block[T](phase: String, pass: Int = 0)(body: => T): T =
    val e = PhaseBlockEvent()
    e.begin()
    e.phase = phase
    e.pass = pass
    try body
    finally e.commit()
