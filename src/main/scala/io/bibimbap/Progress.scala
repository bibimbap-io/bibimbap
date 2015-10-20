package io.bibimbap

import akka.actor._

object Progress {
  def bounded(console: ActorRef, total: Int) = new BoundedProgress(console, total)
}

abstract class Progress {
  def tick(): Unit
}

class BoundedProgress(console: ActorRef, total: Int) {
  var current = 0;

  val minReported = 100
  val reportEvery = 40

  def tick(): Unit = synchronized {
    current += 1;
    if (total > minReported && (current % reportEvery == 0)) {
      val progress = (current*100d)/total
      console ! Out("   "+("%3d".format(progress.toInt))+"% ("+current+"/"+total+")")
    }
  }
}
