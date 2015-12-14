package io.bibimbap

import scala.reflect.ClassTag
import akka.actor._
import akka.util.Timeout
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Await
import scala.concurrent.duration._

trait Module extends Actor with ActorHelpers {
  val settings: Settings
  val console: ActorRef
  val repl: ActorRef

  val name: String

  val dependsOn = Set[String]()
  var modules   = Map[String, ActorRef]()

  def receive: Receive = {
    case Command2("help", command) =>
      if (helpItems contains command) {
        helpItems(command).display(console)
      }
      sender ! CommandSuccess
    case Command1("help" | "?") =>
      for ((command, hi) <- helpItems) {
        helpItems(command).displayShort(console)
      }
      sender ! CommandSuccess

    case os: OnStartup =>
      try {
        startup(os)
        sender ! CommandSuccess
      } catch {
        case e: Exception =>
          sender ! CommandException(e)
      }

    case Complete(buffer, pos) =>
      val (res, index) = completeWithHelp(buffer, pos)
      sender ! Completed(res, index)

    case InputCommand(line) =>
      var foundPartial = false
      for (he <- helpItems.keySet.toSeq.sortBy(-_.length) if !foundPartial && (line startsWith he)) {
        val hi = helpItems(he)
        console ! Error("Incorrect use of "+he+":")
        console ! Error("Usage: "+hi.command+": "+hi.short)

        foundPartial = true;
      }
      if (foundPartial) {
        sender ! CommandSuccess
      } else {
        sender ! CommandUnknown
      }

    case _ =>
      sender ! CommandUnknown
  }

  def complete(buffer: String, pos: Int): (List[String], Int) = {
    (Nil, 0)
  }

  def completeWithHelp(buffer: String, pos: Int): (List[String], Int) = {
    val toComplete = buffer.substring(0, pos)

    val results = for (command <- helpItems.keySet.toList if command.startsWith(toComplete)) yield {
      command+" "
    }

    if (results.isEmpty) {
      complete(buffer, pos)
    } else {
      (results, 0)
    }
  }

  def startup(os: OnStartup) {
    modules = os.modules;

    val depsFound = modules.keySet & dependsOn

    if (depsFound.size < dependsOn.size) {
      for (dep <- dependsOn if !(modules contains dep)) {
        console ! Error("Missing dependency: module '"+name+"' requires module '"+dep+"'")
      }
      repl ! Shutdown
    }
  }

  val helpItems: Map[String, HelpEntry];
}
