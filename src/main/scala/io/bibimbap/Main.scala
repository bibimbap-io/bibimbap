package io.bibimbap

import akka.actor._
import com.typesafe.config.ConfigFactory
import java.util.concurrent.CountDownLatch

object Main {
  def main(args: Array[String]): Unit = {
    val homeDir = System.getProperty("user.home") + System.getProperty("file.separator")

    val configFileName = homeDir + ".bibimbapconfig"
    val historyFileName = homeDir + ".bibimbaphistory"

    val system  = ActorSystem("bibimbap")

    val latch = new CountDownLatch(1)

    val repl = system.actorOf(Props(classOf[Repl], latch, homeDir, configFileName, historyFileName), name = "repl")

    repl ! Start

    latch.await()
    system.shutdown()
    system.awaitTermination()
  }
}
