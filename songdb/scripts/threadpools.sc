// SPDX-License-Identifier: GPL-2.0-or-later AND CC-PDM-1.0
// SPDX-AI-Disclosure: ai-assisted
// Copyright (C) 2026 Matti Tiainen <mvtiaine@cc.hut.fi>

// Shared thread pools for the whole build.
//
//   workerPool       -- CPU work. Nothing submitted here blocks on workerPool itself.
//   orchestratorPool -- tasks that submit to workerPool and then BLOCK waiting for it.
//                       Separate, or an orchestrator blocked behind itself in the FIFO
//                       queue deadlocks.
//
// No I/O pool: I/O is interleaved with CPU in the same task.
//
// `.par` stays on the global ForkJoinPool -- scala.collection.parallel binds defaultTaskSupport
// at collection creation and ignores an implicit EC in scope. A workerPool task may block on the
// FJP (FJP threads never submit back to workerPool). A call site that must keep its parallel
// work off the FJP assigns workerEc through ExecutionContextTaskSupport, and may then block only
// from outside workerPool -- sources/audio.sc does this.
//
// shutdownPools() is called once from songdb.sc; call sites must not shut these down.

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.ThreadFactory
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext

// One worker per core; oversubscribing only adds context switches.
val nThreads: Int = Runtime.getRuntime.availableProcessors

// Named threads so JFR attributes samples to a pool instead of an anonymous worker number.
def namedThreadFactory(prefix: String): ThreadFactory = new ThreadFactory {
  private val counter = new java.util.concurrent.atomic.AtomicInteger(1)
  def newThread(r: Runnable): Thread = {
    val t = new Thread(r, s"$prefix-${counter.getAndIncrement()}")
    // Non-daemon: a worker must not be killed mid-task (half-written TSV).
    // shutdownPools() releases them.
    t.setDaemon(false)
    t
  }
}

// Fixed-size FIFO pool: unbounded queue, no rejections, threads spawned lazily on submit.
def newFifoPool(name: String, size: Int): ThreadPoolExecutor =
  new ThreadPoolExecutor(
    size, size, 0L, TimeUnit.MILLISECONDS,
    new LinkedBlockingQueue[Runnable](),
    namedThreadFactory(name))

// Primary pool: scraping/parsing tasks and the audio duplicate-detection groups.
val workerPool: ThreadPoolExecutor = newFifoPool("songdb-worker", nThreads)

// Tasks that submit to workerPool and block on the results. Threads spawn lazily, so the
// slots left unused by a small component count cost nothing.
val orchestratorPool: ThreadPoolExecutor = newFifoPool("songdb-orchestrator", nThreads)

// ExecutionContext views, for `Future { ... }` / `Await` / `Future.sequence`.
// `workerEc`       -- songdb.sc's source futures (implicit ec there).
// `orchestratorEc` -- futures whose body blocks on workerPool. Currently unused.
val workerEc: ExecutionContext = ExecutionContext.fromExecutor(workerPool)
val orchestratorEc: ExecutionContext = ExecutionContext.fromExecutor(orchestratorPool)

// Teardown, exactly once, from songdb.sc. shutdown() lets queued tasks finish;
// shutdownNow() is the backstop so exit cannot hang forever.
private val pools: Array[ThreadPoolExecutor] = Array(workerPool, orchestratorPool)

def shutdownPools(): Unit = {
  pools.foreach(_.shutdown())
  val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10)
  var attempt = 0
  // Bounded wait: a task stuck on a lock must not turn process exit into a hang.
  while (!pools.forall(_.isTerminated) && System.nanoTime() < deadline) {
    attempt += 1
    if (attempt > 20) pools.foreach(_.shutdownNow())
    Thread.sleep(50)
  }
  if (!pools.forall(_.isTerminated)) pools.foreach(_.shutdownNow())
}
